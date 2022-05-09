module Todo.UI

open System
open Feliz
open Feliz.svg

let renderInput (disabled: bool) (currentText: string) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "space-x"
        prop.style [ style.display.flex ]
        prop.children [
            Html.input [
                prop.valueOrDefault currentText
                prop.onChange (fun s -> dispatch (SetNewTodo s))
                prop.disabled disabled
            ]
            Html.button [
                prop.style [ style.width.minContent ]
                prop.disabled (String.IsNullOrWhiteSpace currentText || disabled)
                prop.ariaBusy disabled
                prop.children [
                    if not disabled then Icons.plus
                ]
                prop.onClick (fun _ -> dispatch AddTodo)
            ]
        ]
    ]


let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
    Html.article [
        prop.key todo.id
        prop.className [
            "space-x"
            if todo.isDone then "dimmed"
        ]
        prop.style [
            style.display.flex
            style.flexDirection.row
        ]
        prop.children [
            Html.span [
                prop.className [
                    if todo.isDone then "line-through"
                ]
                prop.style [
                    style.flexGrow 1
                    style.alignSelf.center
                    style.wordBreak.breakWord
                ]
                prop.text todo.text
                prop.onClick (fun _ -> dispatch (ToggleTodo todo.id))
            ]
            if todo.isDone then
                Html.button [
                    prop.className [ "outline"; "danger" ]
                    prop.style [
                        style.alignSelf.center
                        style.width.minContent
                        style.marginBottom 0
                    ]
                    prop.onClick (fun _ -> dispatch (DeleteTodo todo.id))
                    prop.children [ Icons.trash ]
                ]
            else
                Html.button [
                    prop.className [ "outline" ]
                    prop.style [
                        style.alignSelf.center
                        style.width.minContent
                        style.marginBottom 0
                    ]
                    prop.onClick (fun _ -> dispatch (EditTodo todo.id))
                    prop.children [ Icons.edit ]
                ]
        ]
    ]

let renderEditedTodo (todo: EditedTodo) (dispatch: Msg -> unit) =
    Html.article [
        prop.className "space-x"
        prop.style [
            style.display.flex
            style.flexDirection.row
        ]
        prop.children [
            Html.input [
                prop.style [
                    style.flexGrow 1
                    style.alignSelf.center
                    style.wordBreak.breakWord
                    style.marginBottom 0
                ]
                prop.valueOrDefault todo.text
                prop.onChange (fun s -> dispatch (SetEditedText s))
            ]
            Html.button [
                prop.style [
                    style.alignSelf.center
                    style.width.minContent
                    style.marginBottom 0
                ]
                prop.disabled (
                    todo.text = todo.initial
                    || String.IsNullOrWhiteSpace todo.text
                )
                prop.onClick (fun _ -> dispatch (SaveEditTodo))
                prop.children [ Icons.save ]
            ]
            Html.button [
                prop.className "outline"
                prop.style [
                    style.alignSelf.center
                    style.width.minContent
                    style.marginBottom 0
                ]
                prop.onClick (fun _ -> dispatch (CancelEditTodo))
                prop.children [ Icons.x ]
            ]
        ]
    ]

let renderTodos (state: State) (dispatch: Msg -> unit) =
    match state.todos with
    | Loading -> Html.article [ prop.ariaBusy true ]
    | Failure error -> Html.div [ Html.text error ]
    | Success todos ->
        // render finished todos at the end
        let doneTodos, remainingTodos =
            List.partition (fun todo -> todo.isDone) todos

        Html.div [
            for todo in remainingTodos ->
                match state.editedTodo with
                | Some editedTodo when editedTodo.id = todo.id -> renderEditedTodo editedTodo dispatch
                | _ -> renderTodo todo dispatch
            for todo in doneTodos -> renderTodo todo dispatch
        ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "container"
        prop.children [
            Html.h1 "To-Do List"
            renderInput state.addingNewTodo state.newTodo dispatch
            if not (String.IsNullOrEmpty state.lastError) then
                Html.div [
                    prop.className "danger"
                    prop.text state.lastError
                ]
            renderTodos state dispatch
        ]
    ]
