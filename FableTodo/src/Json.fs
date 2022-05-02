module Todo.Json

open Thoth.Json

module Todo =
    let decoder : Decoder<Todo> =
        Decode.object (fun get ->
            { id = get.Required.Field "_id" Decode.guid
              text = get.Required.Field "text" Decode.string
              isDone = get.Required.Field "isDone" Decode.bool }
        )

module CouchDB =
    let rowsDecoder (decoder : Decoder<'a>) : Decoder<'a list> =
        Decode.field "rows" (Decode.list (Decode.field "doc" decoder))
