module App

open Feliz
open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR

type Todo = { id: int; text: string; isDone: bool }

type EditedTodo = { id: int; text: string }

type State =
    { newTodo: string
      todos: Todo list
      editedTodo: EditedTodo option }

let init () =
    { newTodo = ""
      todos = []
      editedTodo = None }

type Msg =
    | SetNewTodo of string
    | AddTodo
    | ToggleTodo of int
    | DeleteTodo of int
    | EditTodo of int
    | SetEditedText of string
    | CancelEditTodo
    | SaveEditTodo

let update (msg: Msg) (state: State) : State =
    match msg with
    | SetNewTodo text -> { state with newTodo = text }
    | AddTodo ->
        let newId =
            try
                state.todos
                |> List.maxBy (fun todo -> todo.id)
                |> (fun todo -> todo.id + 1)
            with _ -> 0

        { state with
              todos =
                  { id = newId
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
                  |> Option.map (fun todo -> { id = todo.id; text = todo.text }) }
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

let render (state: State) (dispatch: Msg -> unit) =
    let renderInput =
        Html.div [
            prop.className "input-group"
            prop.children [
                Html.input [
                    prop.className "form-control"
                    prop.valueOrDefault state.newTodo
                    prop.onChange (fun s -> dispatch (SetNewTodo s))
                ]
                Html.button [
                    prop.className "btn btn-primary"
                    prop.disabled (System.String.IsNullOrEmpty state.newTodo)
                    prop.children [
                        Html.i [ prop.className "bi bi-plus" ]
                    ]
                    prop.onClick (fun _ -> dispatch AddTodo)
                ]
            ]
        ]

    let renderTodo (todo: Todo) =
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

    let renderEditedTodo (todo: EditedTodo) =
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

    Html.div [
        prop.className "container my-3"
        prop.style [
            style.custom ("maxWidth", "500px")
        ]
        prop.children [
            Html.h1 "To-Do List"

            renderInput

            Html.ul [
                prop.className "list-unstyled"
                // prop.children (List.map renderTodo state.todos)
                prop.children [
                    for todo in state.todos ->
                        match state.editedTodo with
                        | Some editedTodo when editedTodo.id = todo.id -> renderEditedTodo editedTodo
                        | _ -> renderTodo todo
                ]
            ]
        ]
    ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.withDebugger
|> Program.run
