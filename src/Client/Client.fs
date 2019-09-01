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

let update (msg: Msg) (currentModel: Model): Model * Cmd<Msg> =
    match currentModel.Contents, msg with
    | _, Remote(LoadRoot root) -> { currentModel with Contents = Some root }, Cmd.none

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
|> Program.withBridgeConfig
    (
        Bridge.endpoint "/socket/init"
        |> Bridge.withMapping Remote
    )
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
