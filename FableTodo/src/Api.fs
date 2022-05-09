module Todo.Api

open System
open Elmish
open Thoth.Json
open Json

let getTodos (message: RemoteData<Todo list> -> 'msg) : Cmd<'msg> =
    RemoteData.get
        "http://localhost:5984/todos/_all_docs?include_docs=true"
        (Json.CouchDB.rowsDecoder Json.Todo.decoder)
        message

let addTodo (todo: string) (message: Result<Uid, string> -> 'msg) : Cmd<'msg> =
    let guid = Guid.NewGuid()

    let encoded = 
        Encode.object [
            "text", todo
            "isDone", false
        ]

    RemoteData.put
        $"http://localhost:5984/todos/{guid}"
        encoded
        (CouchDB.responseDecoder)
        message

let updateTodo (todo: Todo)  (message: Result<Uid, string> -> 'msg) : Cmd<'msg> =
    let encoded = 
        Encode.object [
            "text", todo.text
            "isDone", todo.isDone
        ]

    RemoteData.put
        $"http://localhost:5984/todos/{todo.id}?rev={todo.rev}"
        encoded
        (CouchDB.responseDecoder)
        message

let deleteTodo (todo: Todo) (message: Result<Uid, string> -> 'msg) : Cmd<'msg> =
    RemoteData.delete
        $"http://localhost:5984/todos/{todo.id}?rev={todo.rev}"
        (CouchDB.responseDecoder)
        message
