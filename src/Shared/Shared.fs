namespace Shared

type Counter = { Value : int }


 /// A type that specifies the messages sent to the server from the client on Elmish.Bridge
/// to learn more, read about at https://github.com/Nhowka/Elmish.Bridge#shared
type ServerMsg =
    | Increment
    | Decrement

/// A type that specifies the messages sent to the client from the server on Elmish.Bridge
type ClientMsg =
    | SyncCounter of Counter
