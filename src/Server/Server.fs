open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared

open Elmish
open Elmish.Bridge

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

type Model =
    { BaseFolder: DirectoryInfo }

let serverHub = ServerHub()

[<System.Security.Permissions.PermissionSet(System.Security.Permissions.SecurityAction.Demand, Name = "FullTrust")>]
let watcher root =
    let watcher = new FileSystemWatcher()
    watcher.Path <- root
    watcher.Filter <- "**"
    watcher.NotifyFilter <-
        NotifyFilters.Size
        ||| NotifyFilters.FileName
        ||| NotifyFilters.DirectoryName
        ||| NotifyFilters.LastWrite
        ||| NotifyFilters.LastAccess
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
    clientDispatch (LoadRoot contents)
    { BaseFolder = di }, Cmd.none

let update clientDispatch msg model =
  try
    match msg with
    | Error ex ->
        eprintfn "%A" ex
        model, Cmd.none
    | ChangedEvent fc ->
        let removeBase' = removeBase model.BaseFolder.FullName
        eprintfn "FullPath: %s\nChangeType: %A" fc.FullPath fc.ChangeType
        match fc.ChangeType with
        | WatcherChangeTypes.Created ->
            let file = FileInfo(fc.FullPath)
            if file.Exists then
              clientDispatch
                (FileCreated
                    {| FullPath = fc.FullPath |> removeBase'
                       Size = int file.Length |})
            else
              let d = {|FullPath =fc.FullPath |> removeBase' ;Children =  readFolder model.BaseFolder.FullName fc.FullPath|}
              let rec inner (dir:{| FullPath: string; Children: FileItem list |})  =
                clientDispatch
                    (dir.FullPath |> removeBase' |> FolderCreated )
                dir.Children |> List.iter (function Directory d -> inner d | File f -> clientDispatch (FileCreated f))
              inner d
        | WatcherChangeTypes.Deleted ->
            clientDispatch ( fc.FullPath |> removeBase' |> FileDeleted)

        | WatcherChangeTypes.Changed ->
            let file = FileInfo(fc.FullPath)
            if file.Exists then
              clientDispatch
                (FileChanged
                    {| FullPath = fc.FullPath |> removeBase'
                       Size = int file.Length |})
        | _ ->
            failwith "Invalid enum for this case"
        model, Cmd.none
    | RenamedEvent fr ->
        let removeBase' = removeBase model.BaseFolder.FullName
        clientDispatch (FileRenamed {| OldName = fr.OldFullPath |> removeBase'
                                       NewName = fr.FullPath |> removeBase'  |})
        model, Cmd.none
    | Remote() -> failwith "Still not needed"
  with ex -> model, Cmd.ofMsg (Error ex)

let webApp =
    let baseFolder =
        Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile), "Shared")
    watcher baseFolder
    Bridge.mkServer "/socket/init" init update
    |> Bridge.withServerHub serverHub
    |> Bridge.runWith Giraffe.server baseFolder

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
