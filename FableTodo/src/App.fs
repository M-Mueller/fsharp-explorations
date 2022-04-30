module App

open System
open Feliz
open Elmish
open Elmish.React

type Todo =
    { id: Guid
      text: string
      isDone: bool }

type EditedTodo =
    { id: Guid
      text: string
      initial: string }

[<RequireQualifiedAccess>]
type Filter =
    | All
    | Completed
    | NotCompleted

type State =
    { newTodo: string
      todos: Todo list
      editedTodo: EditedTodo option
      filter: Filter }

let init () =
    { newTodo = ""
      todos = []
      editedTodo = None
      filter = Filter.All }

type Msg =
    | SetNewTodo of string
    | AddTodo
    | ToggleTodo of Guid
    | DeleteTodo of Guid
    | EditTodo of Guid
    | SetEditedText of string
    | CancelEditTodo
    | SaveEditTodo
    | SetFilter of Filter

let update (msg: Msg) (state: State) : State =
    match msg with
    | SetNewTodo text -> { state with newTodo = text }
    | AddTodo ->
        { state with
              todos =
                  { id = Guid.NewGuid()
                    text = state.newTodo
                    isDone = false }
                  :: state.todos
              newTodo = "" }
    | ToggleTodo id ->
        { state with
              todos =
                  state.todos
                  |> List.map
                      (fun todo ->
                          if todo.id = id then
                              { todo with isDone = not todo.isDone }
                          else
                              todo) }
    | DeleteTodo id ->
        { state with
              todos =
                  state.todos
                  |> List.filter (fun todo -> todo.id <> id) }
    | EditTodo id ->
        { state with
              editedTodo =
                  state.todos
                  |> List.tryFind (fun todo -> todo.id = id)
                  |> Option.map
                      (fun todo ->
                          { id = todo.id
                            text = todo.text
                            initial = todo.text }) }
    | SetEditedText text ->
        match state.editedTodo with
        | Some editedTodo ->
            { state with
                  editedTodo = Some { editedTodo with text = text } }
        | None -> state
    | CancelEditTodo -> { state with editedTodo = None }
    | SaveEditTodo ->
        match state.editedTodo with
        | None -> state
        | Some editedTodo when editedTodo.text = "" -> state
        | Some editedTodo ->
            { state with
                  editedTodo = None
                  todos =
                      state.todos
                      |> List.map
                          (fun todo ->
                              if todo.id = editedTodo.id then
                                  { todo with text = editedTodo.text }
                              else
                                  todo) }
    | SetFilter filter -> { state with filter = filter }


let renderInput (currentText: string) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "input-group my-2"
        prop.children [
            Html.input [
                prop.className "form-control"
                prop.valueOrDefault currentText
                prop.onChange (fun s -> dispatch (SetNewTodo s))
            ]
            Html.button [
                prop.className "btn btn-primary"
                prop.disabled (String.IsNullOrWhiteSpace currentText)
                prop.children [
                    Html.i [ prop.className "bi bi-plus" ]
                ]
                prop.onClick (fun _ -> dispatch AddTodo)
            ]
        ]
    ]

let renderFilters (activeFilter: Filter) (dispatch: Msg -> unit) =
    let renderFilter (text: string) (filter: Filter) =
        Html.button [
            prop.className [
                "btn"
                if activeFilter = filter then
                    "btn-primary"
                else
                    "btn-outline-primary"
            ]
            prop.onClick (fun _ -> dispatch (SetFilter filter))
            prop.text text
        ]

    Html.div [
        prop.className "btn-group w-100 my-2"
        prop.children [
            renderFilter "All" Filter.All
            renderFilter "Completed" Filter.Completed
            renderFilter "Not Completed" Filter.NotCompleted
        ]
    ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
    Html.li [
        prop.className "card my-3"
        prop.children [
            Html.div [
                prop.className "card-body d-flex flex-row"
                prop.children [
                    Html.span [
                        prop.className "flex-grow-1 align-self-center mx-2 text-break"
                        prop.text todo.text
                    ]
                    Html.button [
                        prop.className [
                            "btn"
                            if todo.isDone then
                                "btn-success"
                            else
                                "btn-outline-secondary"
                            "align-self-center"
                            "mx-2"
                        ]
                        prop.onClick (fun _ -> dispatch (ToggleTodo todo.id))
                        prop.children [
                            Html.i [ prop.className "bi bi-check" ]
                        ]
                    ]
                    Html.button [
                        prop.className "btn btn-warning align-self-center mx-2"
                        prop.onClick (fun _ -> dispatch (EditTodo todo.id))
                        prop.children [
                            Html.i [
                                prop.className "bi bi-pencil-square"
                            ]
                        ]
                    ]
                    Html.button [
                        prop.className "btn btn-danger align-self-center mx-2"
                        prop.onClick (fun _ -> dispatch (DeleteTodo todo.id))
                        prop.children [
                            Html.i [ prop.className "bi bi-trash" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderEditedTodo (todo: EditedTodo) (dispatch: Msg -> unit) =
    Html.li [
        prop.className "card my-3"
        prop.children [
            Html.div [
                prop.className "card-body d-flex flex-row"
                prop.children [
                    Html.input [
                        prop.className "flex-grow-1 align-self-center mx-2"
                        prop.valueOrDefault todo.text
                        prop.onChange (fun s -> dispatch (SetEditedText s))
                    ]
                    Html.button [
                        prop.className "btn btn-primary align-self-center mx-2"
                        prop.disabled (
                            todo.text = todo.initial
                            || String.IsNullOrWhiteSpace todo.text
                        )
                        prop.onClick (fun _ -> dispatch (SaveEditTodo))
                        prop.children [
                            Html.i [ prop.className "bi bi-save2" ]
                        ]
                    ]
                    Html.button [
                        prop.className "btn btn-secondary align-self-center mx-2"
                        prop.onClick (fun _ -> dispatch (CancelEditTodo))
                        prop.children [
                            Html.i [ prop.className "bi bi-x" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let renderTodos (state: State) (dispatch: Msg -> unit) =
    let filteredTodos =
        state.todos
        |> List.filter
            (fun todo ->
                match state.filter with
                | Filter.All -> true
                | Filter.Completed -> todo.isDone
                | Filter.NotCompleted -> not todo.isDone)

    Html.ul [
        prop.className "list-unstyled"
        prop.children [
            for todo in filteredTodos ->
                match state.editedTodo with
                | Some editedTodo when editedTodo.id = todo.id -> renderEditedTodo editedTodo dispatch
                | _ -> renderTodo todo dispatch
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "container my-3"
        prop.style [
            style.custom ("maxWidth", "500px")
        ]
        prop.children [
            Html.h1 "To-Do List"
            renderInput state.newTodo dispatch
            renderFilters state.filter dispatch
            renderTodos state dispatch
        ]
    ]


Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
