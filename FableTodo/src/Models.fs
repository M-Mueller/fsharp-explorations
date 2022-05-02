namespace Todo

open System

type Todo =
    { id: Guid
      text: string
      isDone: bool }

type EditedTodo =
    { id: Guid
      text: string
      initial: string }

type State =
    { newTodo: string
      todos: RemoteData<Todo list>
      editedTodo: EditedTodo option }

type Msg =
    | TodosReceived of RemoteData<Todo list>
    | SetNewTodo of string
    | AddTodo
    | ToggleTodo of Guid
    | DeleteTodo of Guid
    | EditTodo of Guid
    | SetEditedText of string
    | CancelEditTodo
    | SaveEditTodo
