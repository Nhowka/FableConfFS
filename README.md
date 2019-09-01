# Remove Server/Client Boundaries with Elmish.Bridge

The Elm architecture lets you think about your program like a conversation.

When you click a button, you tell your update function that "Hey! I just clicked that button. What can you give me for that?" and based on the current model and that message, you get a new model that will reflect in a new view, where you can push another button and have a pleasant talk.

But when you had to talk to your server, it was like having a foreign concept. You weren't just telling "I pressed a button! Show me something new", you were asking "Give me that resource that is on endpoint xyz via the GET method". All the magic of having a state where a new message brings you another state is lost.

So Elmish.Bridge came to the rescue of bringing you back to the pit of success. Don't worry if you should be PUTting or POSTing that data. Just tell "Server! Save that data for me, please?" and if you want you can answer it with "It's saved, my friend! I gave it an id of 345.". The server will communicate with the client with the same messages that you are already comfortable with.

---

## Why would I need Bridge? When it shines?

Bridge was born with a narrower use in mind. The idea was to keep a single model in both client and server, having some messages being handled by the former and the remaining by the latter. It was supposed to only act as a thin interface between a frontend with Fable and a backend with full .NET support.

For the most part, that is how it's best used. It opens new possibilities for libraries that weren't made with Fable in mind. Now you can make some pretty tool with a cool CSS face that can talk with the thousands of packages that NuGet and npm offers.

But besides that, you can now have an Elmish program that runs on the server with full system access. Exploiting that feature, for today we will make an interface for editing files on a server folder.

## Starting out safe

Since version 1.8.0 you can use [SAFE-Template](https://safe-stack.github.io/docs/quickstart/) to create a new project with all you need to start using Elmish.Bridge. This very repository had its first commit with the code you get by typing:

```
dotnet new SAFE -o FableConfFS --communication bridge --deploy heroku
```

Heroku is a nice playground for testing your project online, with less websocket restrictions on a free application and available on SAFE-Tempate since version 1.3.0. Make sure you installed all the pre-requisites and start the little template by opening the `FableConfFS` folder and typing on the console:

```
fake build -t run
```

## Modelling the basics

As we want to share a folder, let's start simple and show that folder contents. For that we will need a folder and a way to represent said folder. For representing the contents we can make a simple union with cases for files and directories:

```fs
// Shared/Shared.fs
type FileItem =
    | Directory of {| FullPath: string; Children: FileItem list |}
    | File of {| FullPath: string; Size: int |}

type ServerMsg = unit

type ClientMsg = LoadRoot of FileItem list
```

We won't send anything to the server at the moment, so we use `unit` as a placeholder for `ServerMsg` and `LoadRoot of FileItem list` for `ClientMsg`. Now we can make the server load the folder when the client connects:

```fs
// Server/Server.fs
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
```

For our messages, we create yet another placeholder. Our model will keep the `DirectoryInfo` of the root folder.

We have a helper function `removeBase` so we don't leak the real location where our files are. The function `readFolder` takes that root folder and the current folder that will read all files and all directories recursively.

The `init` is where we will send the initial state of our shared folder. We create the folder if it not exists and then sends its contents for the client using the `clientDispatch` function and `update` won't be needed right now.

Now we will show that folder content on the client:

```fs
// Client/Client.fs

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

```

For our client we have now a simple model that takes the optional root content that is empty when `init`ially loaded until the server sends it. Once it has come through, the `update` function takes care of getting the content and changing the model with the new content.
The function `sizeFormatter` only makes the file size prettier, `makeTree` creates a simple recursive tree with the contents like the following:

![makeTree result](./Images/SimpleTree.png)

Add some new files to your folder, refresh the page and you shall see the added files on the next start.