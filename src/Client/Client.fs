module Client

open Elmish
open Elmish.React
open Elmish.Bridge
open Fable.React
open Fable.React.Props
open Fulma
open Shared
open Fable.FontAwesome

type Model =
    { Contents: FileItem list option }

type Msg = Remote of ClientMsg

let init(): Model * Cmd<Msg> = { Contents = None }, Cmd.none

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
                       FullPath =
                               target.NewName + d.FullPath.[target.OldName.Length..]
                       Children = d.Children |> List.map replacer |}
        | File f ->
            File
                {| f with FullPath = target.NewName + f.FullPath.[target.OldName.Length..] |}

    let rec inner =
        function
        | File f when f.FullPath.StartsWith target.OldName ->
            File {| f with FullPath = target.NewName + f.FullPath.[target.OldName.Length..] |}
        | Directory d when d.FullPath.StartsWith target.OldName  ->
            Directory {| d with
                            FullPath = target.NewName + d.FullPath.[target.OldName.Length..]
                            Children = d.Children |> List.map replacer |}
        | Directory d when target.OldName.StartsWith d.FullPath ->
            (Directory
                {| d with Children = d.Children |> List.map inner |})
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

let update (msg: Msg) (currentModel: Model): Model * Cmd<Msg> =
    match currentModel.Contents, msg with
    | _, Remote(LoadRoot root) -> { currentModel with Contents = Some root }, Cmd.none
    | None, _ -> currentModel, Cmd.none
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

let rec makeTree (baseFolder: string) dispatch =
    function
    | File f ->
        Field.div
            [ Field.HasAddons
              Field.Props [ Style [ Background "#f5f5ff" ] ] ]
            [ Control.p [] [ Icon.icon [] [ Fa.i [ Fa.Solid.File ] [] ] ]

              Control.p [ Control.IsExpanded ] [ span [] [ str f.FullPath.[baseFolder.Length + 1..] ] ]
              Control.p [] [ str (sizeFormatter f.Size) ] ]
    | Directory d ->
        Field.div []
            [ Control.div []
                  [ span []
                        [ Control.p []
                              [ Icon.icon [] [ Fa.i [ Fa.Solid.FolderOpen ] [] ]
                                str d.FullPath.[baseFolder.Length + 1..] ] ] ]

              Control.div [ Control.IsExpanded ]
                  [ Field.div [ Field.HasAddons ]
                        [ Control.p [ Control.Props [ Style [ Width "15px" ] ] ] []

                          Control.div [ Control.IsExpanded ] (d.Children |> List.map (makeTree d.FullPath dispatch)) ] ] ]

let view (model: Model) (dispatch: Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
              [ Navbar.Item.div [] [ Heading.h2 [] [ str "FableConf File Sharing" ] ] ]

          Container.container []
              (match model.Contents with
               | None -> [ str "Shared folder is being loaded" ]
               | Some content -> content |> List.map (makeTree "" dispatch)) ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withBridgeConfig (Bridge.endpoint "/socket/init" |> Bridge.withMapping Remote)
|> Program.withConsoleTrace
|> Program.withReactBatched "elmish-app"
|> Program.withDebugger
|> Program.run
