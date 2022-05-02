module Todo.App

open System
open Elmish
open Elmish.React
open Elmish.HMR

let init () =
    let getTodos : Cmd<Msg> =
        RemoteData.get
            "http://localhost:5984/todos/_all_docs?include_docs=true"
            (Json.CouchDB.rowsDecoder Json.Todo.decoder)
            TodosReceived

    { newTodo = ""
      todos = Loading
      editedTodo = None },
    getTodos

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | TodosReceived todos -> { state with todos = todos }, Cmd.none
    | SetNewTodo text -> { state with newTodo = text }, Cmd.none
    | AddTodo ->
        let newTodo =
            { id = Guid.NewGuid()
              text = state.newTodo
              isDone = false }

        { state with
              todos = RemoteData.map (fun todos -> newTodo :: todos) state.todos
              newTodo = "" },
        Cmd.none
    | ToggleTodo id ->
        let updateTodo (todo: Todo) =
            if todo.id = id then
                { todo with isDone = not todo.isDone }
            else
                todo

        { state with
              todos = RemoteData.map (List.map updateTodo) state.todos },
        Cmd.none
    | DeleteTodo id ->
        { state with
              todos = RemoteData.map (List.filter (fun todo -> todo.id <> id)) state.todos },
        Cmd.none
    | EditTodo id ->
        match state.todos with
        | Success todos ->
            { state with
                  editedTodo =
                      todos
                      |> List.tryFind (fun todo -> todo.id = id)
                      |> Option.map
                          (fun todo ->
                              { id = todo.id
                                text = todo.text
                                initial = todo.text }) },
            Cmd.none
        | _ -> state, Cmd.none
    | SetEditedText text ->
        match state.editedTodo with
        | Some editedTodo ->
            { state with
                  editedTodo = Some { editedTodo with text = text } },
            Cmd.none
        | None -> state, Cmd.none
    | CancelEditTodo -> { state with editedTodo = None }, Cmd.none
    | SaveEditTodo ->
        match state.editedTodo with
        | None -> state, Cmd.none
        | Some editedTodo when editedTodo.text = "" -> state, Cmd.none
        | Some editedTodo ->
            let updateTodo (todo: Todo) =
                if todo.id = editedTodo.id then
                    { todo with text = editedTodo.text }
                else
                    todo

            { state with
                  editedTodo = None
                  todos = RemoteData.map (List.map updateTodo) state.todos },
            Cmd.none


Program.mkProgram init update UI.render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
