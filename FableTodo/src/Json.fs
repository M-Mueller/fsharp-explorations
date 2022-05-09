module Todo.Json

open System
open Thoth.Json

module Todo =
    let decoder : Decoder<Todo> =
        Decode.object
            (fun get ->
                { id = get.Required.Field "_id" Decode.string
                  rev = get.Required.Field "_rev" Decode.string
                  text = get.Required.Field "text" Decode.string
                  isDone = get.Required.Field "isDone" Decode.bool })

    let encode (todo: Todo) : JsonValue =
        Encode.object [
            "text", todo.text
            "isDone", todo.isDone
        ]

module CouchDB =
    let rowsDecoder (decoder: Decoder<'a>) : Decoder<'a list> =
        Decode.field "rows" (Decode.list (Decode.field "doc" decoder))

    let responseDecoder : Decoder<Uid> =
        Decode.object (fun get -> get.Required.Field "id" Decode.string)
