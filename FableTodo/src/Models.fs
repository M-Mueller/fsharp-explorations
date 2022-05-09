namespace Todo

open System

type Uid = string

type Todo =
    { id: Uid
      rev: string
      text: string
      isDone: bool }

type EditedTodo =
    { id: Uid
      text: string
      initial: string }

type State =
    { newTodo: string
      addingNewTodo: bool
      todos: RemoteData<Todo list>
      editedTodo: EditedTodo option
      lastError: string }

type Msg =
    | TodosReceived of RemoteData<Todo list>
    | SetNewTodo of string
    | AddTodo
    | TodoAdded of Result<Uid, string>
    | ToggleTodo of Uid
    | DeleteTodo of Uid
    | EditTodo of Uid
    | SetEditedText of string
    | CancelEditTodo
    | SaveEditTodo
    | TodoChanged of Result<Uid, string>
