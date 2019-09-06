module Client

open Elmish
open Elmish.React
open Elmish.Bridge
open Fable.React
open Fable.Core
open Fable.React.Props
open Fulma
open Shared
open Fable.FontAwesome
open Browser.Types
open Fable.Core.Util
open Fable.Core.JsInterop


type Model =
    { Contents: FileItem list option
      UserGuid: System.Guid option
      UploadLinks: Map<string, System.Guid>
      DownloadLinks: Map<string, Choice<string, System.Guid>>
      Upload: string option
      CreatingFolder: string option }

type Msg =
    | Remote of ClientMsg
    | OpenUpload of string
    | CloseUpload
    | OpenCreateFolder of string
    | CloseCreateFolder
    | ClearDownload of string

let init(): Model * Cmd<Msg> =
    { Contents = None
      UserGuid = None
      UploadLinks = Map.empty
      DownloadLinks = Map.empty
      Upload = None
      CreatingFolder = None }, Cmd.none

let delete target files =
    let rec inner =
        function
        | File f when f.FullPath = target -> None
        | Directory d when d.FullPath = target -> None
        | Directory d when target.StartsWith d.FullPath ->
            Some(Directory {| d with Children = d.Children |> List.choose inner |})
        | any -> Some any
    files |> List.choose inner

let change (target: {| FullPath: string; Size: int |}) files =
    let rec inner =
        function
        | File f when f.FullPath = target.FullPath -> File target
        | Directory d when target.FullPath.StartsWith d.FullPath ->
            (Directory {| d with Children = d.Children |> List.map inner |})
        | any -> any
    files |> List.map inner

let rename (target: {| NewName: string; OldName: string |}) files =
    let rec replacer =
        function
        | Directory d ->
            Directory
                {| d with
                       FullPath = target.NewName + d.FullPath.[target.OldName.Length..]
                       Children = d.Children |> List.map replacer |}
        | File f -> File {| f with FullPath = target.NewName + f.FullPath.[target.OldName.Length..] |}

    let rec inner =
        function
        | File f when f.FullPath.StartsWith target.OldName ->
            File {| f with FullPath = target.NewName + f.FullPath.[target.OldName.Length..] |}
        | Directory d when d.FullPath.StartsWith target.OldName ->
            Directory
                {| d with
                       FullPath = target.NewName + d.FullPath.[target.OldName.Length..]
                       Children = d.Children |> List.map replacer |}
        | Directory d when target.OldName.StartsWith d.FullPath ->
            (Directory {| d with Children = d.Children |> List.map inner |})
        | any -> any

    files |> List.map inner

let create (target: {| FullPath: string; Size: int |}) files =
    let rec inner f =
        if f
           |> List.exists (function
               | Directory d when target.FullPath.StartsWith d.FullPath -> true
               | _ -> false)
        then
            f
            |> List.map (function
                | Directory d when target.FullPath.StartsWith d.FullPath ->
                    Directory {| d with Children = inner d.Children |}
                | any -> any)
        else
            (File target) :: f
            |> List.sortBy (function
                | File f -> f.FullPath
                | Directory d -> d.FullPath)
    inner files

let createFolder (target: string) files =
    let rec inner f =
        if f
           |> List.exists (function
               | Directory d when target.StartsWith d.FullPath -> true
               | _ -> false)
        then
            f
            |> List.map (function
                | Directory d when target.StartsWith d.FullPath -> Directory {| d with Children = inner d.Children |}
                | any -> any)
        else
            (Directory
                {| FullPath = target
                   Children = [] |}) :: f
            |> List.sortBy (function
                | File f -> f.FullPath
                | Directory d -> d.FullPath)
    inner files


let uploadSmallFile folder (file: Browser.Types.File) =
    let reader = Browser.Dom.FileReader.Create()
    let path = sprintf "%s/%s" folder file.name
    reader.onload <-
        (fun _ ->
        let b =
            reader.result
            |> unbox
            |> System.Convert.ToBase64String
        Bridge.Send(UploadSmallFile(path, b)))
    reader.readAsArrayBuffer (file)

let update (msg: Msg) (currentModel: Model): Model * Cmd<Msg> =
    match currentModel.Contents, msg with
    | _, Remote(LoadRoot root) -> { currentModel with Contents = Some root }, Cmd.none
    | _, Remote(SetGuid guid) ->
        { currentModel with
              UserGuid = Some guid
              DownloadLinks = Map.empty
              UploadLinks = Map.empty
              Upload = None }, Cmd.none
    | _, Remote(UploaderReady(file, guid)) ->
        { currentModel with UploadLinks = currentModel.UploadLinks |> Map.add file guid }, Cmd.none
    | _, Remote(DownloadBigFile(file, guid)) ->
        { currentModel with DownloadLinks = currentModel.DownloadLinks |> Map.add file (Choice2Of2 guid) }, Cmd.none
    | _, ClearDownload file ->
        { currentModel with DownloadLinks = currentModel.DownloadLinks |> Map.remove file }, Cmd.none
    | _, Remote(DownloadSmallFile(file, content)) ->
        { currentModel with DownloadLinks = currentModel.DownloadLinks |> Map.add file (Choice1Of2 content) }, Cmd.none
    | _, CloseUpload -> { currentModel with Upload = None }, Cmd.none
    | None, _ -> currentModel, Cmd.none
    | Some _, OpenCreateFolder s -> { currentModel with CreatingFolder = Some s }, Cmd.none
    | Some _, CloseCreateFolder -> { currentModel with CreatingFolder = None }, Cmd.none
    | Some _, OpenUpload s ->

        { currentModel with Upload = Some s }, Cmd.none
    | Some content, Remote(FileDeleted file) -> { currentModel with Contents = Some(delete file content) }, Cmd.none
    | Some content, Remote(FileRenamed file) -> { currentModel with Contents = Some(rename file content) }, Cmd.none
    | Some content, Remote(FileChanged file) -> { currentModel with Contents = Some(change file content) }, Cmd.none
    | Some content, Remote(FileCreated file) -> { currentModel with Contents = Some(create file content) }, Cmd.none
    | Some content, Remote(FolderCreated folder) ->
        { currentModel with Contents = Some(createFolder folder content) }, Cmd.none

let sizeFormatter =
    let sizes = [ "KB"; "MB"; "GB" ]
    fun n ->
        if n > 1024 then
            let rec formatter sizes n =
                match sizes with
                | current :: remaining ->
                    if n > 1024. then formatter remaining (n / 1024.)
                    else sprintf "%0.2f%s" n current
                | [] -> "Big"
            formatter sizes ((float n) / 1024.)
        else sprintf "%iB" n

let rec showTree content (baseFolder: string) model dispatch =
    let makeTree =
        function
        | File f ->
            Field.div
                [ Field.HasAddonsRight
                  Field.Props [ Style [ Background "#f5f5ff" ] ] ]
                [ Control.p [] [ Icon.icon [] [ Fa.i [ Fa.Solid.File ] [] ] ]

                  Control.p [ Control.IsExpanded ] [ span [] [ str f.FullPath.[baseFolder.Length + 1..] ] ]
                  Control.p []
                      [ span []
                            [ yield str (sizeFormatter f.Size)
                              let file = model.DownloadLinks |> Map.tryFind f.FullPath
                              let name = f.FullPath.[baseFolder.Length + 1..]
                              match model.UserGuid with
                              | Some userGuid ->
                                  yield Icon.icon []
                                            [ a
                                                [ match file with
                                                  | None ->
                                                      yield Style [ Color "#4a4a4a" ]
                                                      yield OnClick(fun _ -> Bridge.Send(RequestDownload f.FullPath))
                                                  | Some fd ->
                                                      yield Style [ Color "green" ]
                                                      yield Download name
                                                      match fd with
                                                      | Choice1Of2 content ->
                                                          yield Href
                                                                    (sprintf "data:application/octet-stream;base64,%s"
                                                                         content)
                                                      | Choice2Of2 g ->
                                                          yield Target "_blank"
                                                          yield Href(sprintf "/api/Download/%O/%O" userGuid g)
                                                          yield OnClick(fun _ -> dispatch (ClearDownload f.FullPath)) ]
                                                  [ Fa.i [ Fa.Solid.Download ] [] ] ]
                              | None -> () ] ] ]
        | Directory d ->
            Field.div []
                [ Control.div []
                      [ Field.div []
                            [ Control.div [ Control.IsExpanded ]
                                  [ span []
                                        [ Control.p []
                                              [ Icon.icon [] [ Fa.i [ Fa.Solid.FolderOpen ] [] ]
                                                str d.FullPath.[baseFolder.Length + 1..] ] ] ] ] ]
                  Control.div [ Control.IsExpanded ]
                      [ Field.div [ Field.HasAddons ]
                            [ span [ Style [ Width "15px" ] ] []
                              showTree d.Children d.FullPath model dispatch ] ] ]

    Container.container []
        [ Field.div [ Field.IsHorizontal ]
              [ Button.a
                  [ Button.IsFullWidth
                    Button.Size IsSmall
                    Button.OnClick(fun _ ->
                        if model.UploadLinks
                           |> Map.containsKey baseFolder
                           |> not
                        then Bridge.Send(RequestUpload baseFolder)
                        dispatch (OpenUpload baseFolder)) ]
                    [ Icon.icon [] [ Fa.i [ Fa.Solid.Upload ] [] ]
                      span [] [ str "Upload file" ] ]


                Button.a
                    [ Button.Size IsSmall
                      Button.IsFullWidth
                      Button.OnClick(fun _ -> dispatch (OpenCreateFolder baseFolder)) ]
                    [ Icon.icon [] [ Fa.i [ Fa.Solid.FolderPlus ] [] ]
                      span [] [ str "Create folder" ] ] ]

          Control.div
              [ Control.Props [ Style [ Position PositionOptions.Relative ] ]
                Control.IsExpanded ]
              [
              for e in content do
                  yield makeTree e ] ]


[<Emit("[].slice.call(new Uint8Array($0))")>]
let toArray b: byte [] = jsNative

let uploadForm =
    FunctionComponent.Of
        ((fun (props: {| dispatch: Msg -> unit; userGuid: System.Guid; folderGuid: System.Guid option; folderPath: string |}) ->
         let filesState: IStateHook<File list> = Hooks.useState []

         let smallUpload =
             match filesState.current with
             | [] -> false
             | [ e ] when e.size < (4 <<< 10) -> true
             | _ -> false

         form
             [ if not smallUpload then
                 yield Action(sprintf "/api/Upload/%O/%O" props.userGuid props.folderGuid)
                 yield Target "hidden-iframe"
                 yield EncType "multipart/form-data"
                 yield Method "post" ]
             [ Field.div [ Field.HasAddons ]
                   [ Control.div [ Control.IsExpanded ]
                         [ File.file [ File.IsFullWidth; File.HasName ]
                               [ File.label []
                                     [ File.input
                                         [ Props
                                             [ OnChange(fun e ->
                                                 let fl: FileList = e.target?files
                                                 filesState.update ([ for i in 0..fl.length - 1 -> fl.item i ]))
                                               Required true
                                               Multiple true
                                               Name "files" ] ]
                                       File.cta [] [ File.icon [] [ Icon.icon [] [ Fa.i [ Fa.Solid.Upload ] [] ] ] ] ] ] ]
                     Control.div [ Control.IsExpanded ]
                         [ match filesState.current with
                           | [] -> yield str "No file"
                           | e ->
                               yield Control.p []
                                         [ for f in e do
                                             yield Field.p [] [ str f.name ] ] ]
                     Control.div []
                         [ (if smallUpload then Button.a
                            else Button.button)
                             [ Button.Disabled(not smallUpload && props.folderGuid.IsNone)
                               Button.OnClick(fun _ ->
                                   if smallUpload then
                                       filesState.current
                                       |> Seq.tryHead
                                       |> Option.iter (fun f ->
                                           let fr = Browser.Dom.FileReader.Create()
                                           fr.onload <-
                                               (fun _ ->
                                               let res =
                                                   fr.result
                                                   |> toArray
                                                   |> System.Convert.ToBase64String
                                               Bridge.Send
                                                   (UploadSmallFile(sprintf "/%s/%s" props.folderPath f.name, res)))
                                           fr.readAsArrayBuffer f)
                                   props.dispatch CloseUpload) ]
                               [ str
                                   (if smallUpload then "Inline send"
                                    else "Form send") ] ] ] ]), memoizeWith = equalsButFunctions)

let createFolderForm =
    FunctionComponent.Of
        ((fun (props: {| dispatch: Msg -> unit; folderPath: string |}) ->
         let folderState = Hooks.useState ""


         Field.div []
             [ Label.label [] [ str "New folder: " ]
               Field.div [ Field.HasAddons ]
                   [ if props.folderPath <> "" then
                       yield Control.div [] [ Button.a [ Button.IsStatic true ] [ str props.folderPath ] ]
                     yield Control.div [ Control.IsExpanded ]
                               [ Input.input
                                   [ Input.ValueOrDefault folderState.current
                                     Input.OnChange(fun e ->
                                         let x = e.Value.Trim('/', '\\')
                                         folderState.update (x))
                                     Input.Placeholder "Choose a name for the new folder" ] ]
                     yield Control.div []
                               [ Button.a
                                   [ Button.OnClick
                                       (fun _ ->
                                       if folderState.current <> "" then
                                           props.dispatch CloseCreateFolder
                                           Bridge.Send
                                               (CreateFolder(sprintf "%s/%s" props.folderPath folderState.current))) ]
                                     [ str "Create" ] ] ] ]), memoizeWith = equalsButFunctions)

let view (model: Model) (dispatch: Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
              [ Navbar.Item.div [] [ Heading.h2 [] [ str "FableConf File Sharing" ] ] ]

          Container.container []
              [ match model.Contents with
                | None -> yield str "Shared folder is being loaded"
                | Some content -> yield showTree content "" model dispatch ]

          Modal.modal [ Modal.IsActive model.Upload.IsSome ]
              [ Modal.background [ Props [ OnClick(fun _ -> dispatch CloseUpload) ] ] []
                Modal.content []
                    [ Box.box' []
                          [ match model.UserGuid, model.Upload with
                            | None, _ -> yield str "Loading user id"
                            | _, None -> yield str "Folder reference lost"
                            | Some g, Some fg ->
                                yield uploadForm
                                          {| dispatch = dispatch
                                             userGuid = g
                                             folderGuid = model.UploadLinks |> Map.tryFind fg
                                             folderPath = fg |} ] ]
                Modal.close
                    [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick(fun _ -> dispatch CloseUpload) ] [] ]

          Modal.modal [ Modal.IsActive model.CreatingFolder.IsSome ]
              [ Modal.background [ Props [ OnClick(fun _ -> dispatch CloseCreateFolder) ] ] []
                Modal.content []
                    [ Box.box' []
                          [ match model.CreatingFolder with
                            | None -> yield str "Loading user id"
                            | Some baseFolder ->
                                yield createFolderForm
                                          {| dispatch = dispatch
                                             folderPath = baseFolder |} ] ]
                Modal.close
                    [ Modal.Close.Size IsLarge
                      Modal.Close.OnClick(fun _ -> dispatch CloseCreateFolder) ] [] ]
          iframe
              [ Style [ Display DisplayOptions.None ]
                Name "hidden-iframe" ] [] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withBridgeConfig (Bridge.endpoint "/socket/init" |> Bridge.withMapping Remote)
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
