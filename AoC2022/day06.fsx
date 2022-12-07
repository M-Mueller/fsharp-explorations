#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

let numCharactersUntilMarker (markerLength: int) (stream: string) : int =
    let markerIndex =
        stream
        |> Seq.windowed markerLength
        |> Seq.findIndex (fun chars -> Set(chars).Count = markerLength)

    markerIndex + markerLength


//////////////////////////
// Tests
//////////////////////////

let runTests =
    test <@ "mjqjpqmgbljsphdztnvjfqwrcgsmlb" |> numCharactersUntilMarker 4 = 7 @>
    test <@ "bvwbjplbgvbhsrlpgdmjqwftvncz" |> numCharactersUntilMarker 4 = 5 @>
    test <@ "nppdvjthqldpwncqszvftbrmjlhg" |> numCharactersUntilMarker 4 = 6 @>
    test <@ "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" |> numCharactersUntilMarker 4 = 10 @>
    test <@ "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |> numCharactersUntilMarker 4 = 11 @>

    test <@ "mjqjpqmgbljsphdztnvjfqwrcgsmlb" |> numCharactersUntilMarker 14 = 19 @>
    test <@ "bvwbjplbgvbhsrlpgdmjqwftvncz" |> numCharactersUntilMarker 14 = 23 @>
    test <@ "nppdvjthqldpwncqszvftbrmjlhg" |> numCharactersUntilMarker 14 = 23 @>
    test <@ "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" |> numCharactersUntilMarker 14 = 29 @>
    test <@ "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |> numCharactersUntilMarker 14 = 26 @>

//////////////////////////
// AoC
//////////////////////////

let datastream = IO.File.ReadLines("day06.in") |> Seq.head

datastream
|> numCharactersUntilMarker 4
|> printfn "%d characters need to be processed for the start-of-packet"

datastream
|> numCharactersUntilMarker 14
|> printfn "%d characters need to be processed for the start-of-message"
