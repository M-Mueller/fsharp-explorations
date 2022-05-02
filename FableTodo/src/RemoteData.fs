namespace Todo

open System
open Elmish
open Thoth.Json
open Fable.SimpleHttp

type RemoteData<'a> =
    | Loading
    | Failure of string
    | Success of 'a

module RemoteData =
    let map f data =
        match data with
        | Loading -> Loading
        | Failure error -> Failure error
        | Success value -> Success (f value)

    let internal fromAsync (operation: Async<'msg>) : Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit) : unit =
            let delayedDispatch =
                async {
                    let! msg = operation
                    dispatch msg
                }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd


    let inline get url (decoder: Decoder<'data>) (message: RemoteData<'data> -> 'msg) : Cmd<'msg> =
        async {
            let credentials = Convert.ToBase64String (Text.Encoding.UTF8.GetBytes("admin:password"))

            let! response =
                Http.request url
                |> Http.method GET
                |> Http.header (Headers.authorization $"Basic {credentials}")
                |> Http.send

            let data =
                if response.statusCode = 200 then
                    let decoded = Decode.fromString decoder response.responseText

                    match decoded with
                    | Ok user -> Success user
                    | Error error -> Failure $"Decoding failed with {error}"
                else
                    Failure $"Requesting {url} failed with code {response.statusCode}"

            return message data
        }
        |> fromAsync
