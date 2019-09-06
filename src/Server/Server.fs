open System.IO
open System.Threading
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared

open Elmish
open Elmish.Bridge
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Features

let tryGetEnv =
    System.Environment.GetEnvironmentVariable
    >> function
    | null
    | "" -> None
    | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "PORT"
    |> tryGetEnv
    |> Option.map uint16
    |> Option.defaultValue 8085us

type Msg =
    | Remote of ServerMsg
    | ChangedEvent of FileSystemEventArgs
    | RenamedEvent of RenamedEventArgs
    | Error of exn
    | DeleteTempFiles
    | Callback of ClientMsg
    | FileReady of string * System.Guid * string

type Model =
    { BaseFolder: DirectoryInfo
      UserGuid: System.Guid
      Uploads: Map<System.Guid, string>
      Downloads: Map<System.Guid, string> }

let serverHub = ServerHub()


[<System.Security.Permissions.PermissionSet(System.Security.Permissions.SecurityAction.Demand, Name = "FullTrust")>]
let watcher root =
    let watcher = new FileSystemWatcher()
    watcher.Path <- root
    watcher.Filter <- "*"
    watcher.NotifyFilter <- NotifyFilters.Size ||| NotifyFilters.FileName ||| NotifyFilters.DirectoryName
    watcher.IncludeSubdirectories <- true
    let changes _ e = serverHub.BroadcastServer(ChangedEvent e)
    let renamed _ e = serverHub.BroadcastServer(RenamedEvent e)
    watcher.Changed.AddHandler(FileSystemEventHandler changes)
    watcher.Created.AddHandler(FileSystemEventHandler changes)
    watcher.Deleted.AddHandler(FileSystemEventHandler changes)
    watcher.Renamed.AddHandler(RenamedEventHandler renamed)
    watcher.EnableRaisingEvents <- true



let removeBase (baseFolder: string) (fullName: string) =
    if fullName.StartsWith baseFolder then fullName.[baseFolder.Length..]
    else fullName

let rec readFolder baseFolder folder =
    let di = DirectoryInfo folder
    let files =
        di.GetFiles() |> Array.map (fun e -> File
                                                 {| FullPath = e.FullName |> removeBase baseFolder
                                                    Size = int e.Length |})
    let dirs =
        di.GetDirectories() |> Array.map (fun d -> Directory
                                                       {| FullPath = d.FullName |> removeBase baseFolder
                                                          Children = readFolder baseFolder d.FullName |})
    Array.append files dirs
    |> Array.sortBy (function
        | File f -> f.FullPath
        | Directory d -> d.FullPath)
    |> Array.toList

let init clientDispatch baseFolder =
    let di = DirectoryInfo baseFolder
    if not di.Exists then di.Create()
    let contents = readFolder di.FullName di.FullName
    let g = System.Guid.NewGuid()
    clientDispatch (SetGuid g)
    clientDispatch (LoadRoot contents)

    { BaseFolder = di
      Uploads = Map.empty
      Downloads = Map.empty
      UserGuid = g }, Cmd.none

let update clientDispatch msg model =
    try
        let removeBase' = removeBase model.BaseFolder.FullName
        let addBase (s: string) = Path.Combine(model.BaseFolder.FullName, s.TrimStart('\\', '/'))
        match msg with
        | Error ex ->
            eprintfn "%A" ex
            model, Cmd.none
        | Callback c ->
            clientDispatch c
            model, Cmd.none
        | ChangedEvent fc ->
            match fc.ChangeType with
            | WatcherChangeTypes.Created ->
                let file = FileInfo(fc.FullPath)

                if file.Exists then
                    clientDispatch
                        (FileCreated
                            {| FullPath = fc.FullPath |> removeBase'
                               Size = int file.Length |})
                else
                    let d =
                        {| FullPath = fc.FullPath |> removeBase'
                           Children = readFolder model.BaseFolder.FullName fc.FullPath |}

                    let rec inner (dir: {| FullPath: string; Children: FileItem list |}) =
                        clientDispatch
                            (dir.FullPath
                             |> removeBase'
                             |> FolderCreated)
                        dir.Children
                        |> List.iter (function
                            | Directory d -> inner d
                            | File f -> clientDispatch (FileCreated f))
                    inner d
            | WatcherChangeTypes.Deleted ->
                clientDispatch
                    (fc.FullPath
                     |> removeBase'
                     |> FileDeleted)

            | WatcherChangeTypes.Changed ->
                let file = FileInfo(fc.FullPath)
                if file.Exists then
                    clientDispatch
                        (FileChanged
                            {| FullPath = fc.FullPath |> removeBase'
                               Size = int file.Length |})
            | _ -> failwith "Invalid enum for this case"
            model, Cmd.none
        | RenamedEvent fr ->
            clientDispatch
                (FileRenamed
                    {| OldName = fr.OldFullPath |> removeBase'
                       NewName = fr.FullPath |> removeBase' |})
            model, Cmd.none
        | Remote cmsg ->
            match cmsg with
            | RequestDownload s ->
                let fi =
                    s
                    |> addBase
                    |> FileInfo
                if fi.Exists then
                    if fi.Length < (4L <<< 10) then
                        let r =
                            async {
                                use fs = fi.OpenRead()
                                use br = new BinaryReader(fs)
                                let b = br.ReadBytes(int fi.Length)
                                return Callback(DownloadSmallFile(s, System.Convert.ToBase64String b))
                            }
                        model, Cmd.OfAsync.result r
                    else
                        let r =
                            async {
                                let g = System.Guid.NewGuid()
                                let t = Path.GetTempFileName()
                                use fs = fi.OpenRead()
                                use ts = File.Create t
                                do! fs.CopyToAsync ts |> Async.AwaitTask
                                return FileReady (s, g, t)
                            }
                        model, Cmd.OfAsync.result r
                else model, Cmd.none
            | RequestUpload d ->
                let g = System.Guid.NewGuid()
                { model with Uploads = model.Uploads |> Map.add g d }, Cmd.ofMsg (Callback(UploaderReady(d, g)))

            | UploadSmallFile(file, content) ->
                let file = addBase file
                let fi = FileInfo file
                let c = System.Convert.FromBase64String content
                use ms = new MemoryStream(c)
                if fi.Exists then fi.Delete()
                use fs = fi.Create()
                ms.CopyTo fs
                model, Cmd.none
            | CreateFolder f ->
                let di = addBase f |> DirectoryInfo
                if not di.Exists then di.Create()
                model, Cmd.none
        | DeleteTempFiles ->
            model.Downloads
            |> Map.iter (fun _ t ->
                let fi = FileInfo t
                if fi.Exists then fi.Delete())

            { model with Downloads = Map.empty }, Cmd.none
        | FileReady(s, g, di) ->
            { model with Downloads = model.Downloads |> Map.add g di }, Cmd.ofMsg (Callback(DownloadBigFile(s, g)))
    with ex -> model, Cmd.ofMsg (Error ex)


let uploadHandler baseFolder (uid: System.Guid, fid: System.Guid): HttpHandler =
    fun next ctx ->
        task {
            let folder =
                serverHub.GetModels()
                |> List.tryPick (fun { UserGuid = u; Uploads = f } ->
                    if uid = u then f |> Map.tryFind fid
                    else None)
            match folder with
            | Some f ->
                let folder = Path.Combine(baseFolder, f.TrimStart('\\', '/'))
                let di = DirectoryInfo folder

                if not di.Exists then di.Create()
                let formFeature = ctx.Features.Get<IFormFeature>()
                let! form = formFeature.ReadFormAsync CancellationToken.None
                for file in form.Files do
                    let fd = Path.Combine(di.FullName, file.FileName)
                    use fs = File.Create(fd)
                    do! file.CopyToAsync fs
                return! text "OK" next ctx
            | None -> return None
        }

let downloadHandler (uid: System.Guid, fid: System.Guid): HttpHandler =
    fun next ctx ->
        task {
            let file =
                serverHub.GetModels()
                |> List.tryPick (fun { UserGuid = u; Downloads = f } ->
                    if uid = u then f |> Map.tryFind fid
                    else None)
            match file with
            | Some f ->
                let fi = FileInfo f
                if fi.Exists then
                    use fs =
                        new FileStream(f, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite, 4096,
                                       FileOptions.DeleteOnClose)
                    return! ctx.WriteStreamAsync true fs None None
                else return None
            | None -> return None
        }

let webApp: HttpHandler =
    let userFolder = System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile)
    let baseFolder = Path.Combine(userFolder, "Shared")
    let folder = DirectoryInfo baseFolder
    if not folder.Exists then folder.Create()
    watcher baseFolder
    choose
        [ routef "/api/Download/%O/%O" downloadHandler
          routef "/api/Upload/%O/%O" (uploadHandler baseFolder)
          Bridge.mkServer "/socket/init" init update
          |> Bridge.withServerHub serverHub
          |> Bridge.whenDown DeleteTempFiles
          |> Bridge.runWith Giraffe.server baseFolder ]

let app =
    application {
        url ("http://0.0.0.0:" + port.ToString() + "/")
        use_router webApp
        memory_cache
        use_static publicPath
        use_json_serializer (Thoth.Json.Giraffe.ThothSerializer())
        app_config Giraffe.useWebSockets
        use_gzip
    }

run app
