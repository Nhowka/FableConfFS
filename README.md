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
