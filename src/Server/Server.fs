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

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

/// Elmish init function with a channel for sending client messages
/// Returns a new state and commands
let init clientDispatch () =
    let value = { Value = 42 }
    clientDispatch (SyncCounter value)
    value, Cmd.none

/// Elmish update function with a channel for sending client messages
/// Returns a new state and commands
let update clientDispatch msg model =
    match msg with
    | Increment ->
        let newModel = { model with Value = model.Value + 1 }
        clientDispatch (SyncCounter newModel)
        newModel, Cmd.none
    | Decrement ->
        let newModel = { model with Value = model.Value - 1 }
        clientDispatch (SyncCounter newModel)
        newModel, Cmd.none

/// Connect the Elmish functions to an endpoint for websocket connections
let webApp =
    Bridge.mkServer "/socket/init" init update
    |> Bridge.run Giraffe.server

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    app_config Giraffe.useWebSockets
    use_gzip
}

run app
