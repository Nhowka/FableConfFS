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

type Msg = Remote of ServerMsg

type Model =
    { BaseFolder: DirectoryInfo }

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
    match msg with
    | Remote() -> failwith "Still not needed"


let webApp =
    let baseFolder =
        Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile), "Shared")

    Bridge.mkServer "/socket/init" init update |> Bridge.runWith Giraffe.server baseFolder

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
