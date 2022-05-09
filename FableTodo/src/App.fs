module Todo.App

open System
open Elmish
open Elmish.React
open Elmish.HMR

let init () =
    { newTodo = ""
      addingNewTodo = false
      todos = Loading
      editedTodo = None
      lastError = "" },
    (Api.getTodos TodosReceived)

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    let findTodo id =
        state.todos
        |> RemoteData.getSuccess
        |> Option.defaultValue []
        |> List.filter (fun todo -> todo.id = id)
        |> List.tryHead

    match msg with
    | TodosReceived todos -> { state with todos = todos }, Cmd.none
    | SetNewTodo text -> { state with newTodo = text }, Cmd.none
    | AddTodo -> ({ state with addingNewTodo = true }, Api.addTodo state.newTodo TodoAdded)
    | TodoAdded data ->
        match data with
        | Ok _ ->
            ({ state with
                   addingNewTodo = false
                   newTodo = ""
                   lastError = ""
                   todos = Loading },
             Api.getTodos TodosReceived)
        | Error error ->
            ({ state with
                   addingNewTodo = false
                   lastError = error },
             Cmd.none)
    | ToggleTodo id ->
        (state,
         match findTodo id with
         | Some todo -> Api.updateTodo { todo with isDone = not todo.isDone } TodoChanged
         | None -> Cmd.none)
    | DeleteTodo id ->
        (state,
         match findTodo id with
         | Some todo -> Api.deleteTodo todo TodoChanged
         | None -> Cmd.none)
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
            ({ state with editedTodo = None },
             match findTodo editedTodo.id with
             | Some todo -> Api.updateTodo { todo with text = editedTodo.text } TodoChanged
             | None -> Cmd.none)
    | TodoChanged result ->
        match result with
        | Ok _ ->
            ({ state with
                   todos = Loading
                   lastError = "" },
             Api.getTodos TodosReceived)
        | Error error -> ({ state with lastError = error }, Cmd.none)


Program.mkProgram init update UI.render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
