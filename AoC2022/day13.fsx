#r "nuget: Unquote, 6.1.0"

#load "Common.fsx"

open Swensen.Unquote
open System
open Common


type PacketData =
    | DataList of PacketData list
    | Number of int


let parsePacketData line = Number 0


let parseInput filename =
    IO.File.ReadLines(filename)
    |> Seq.toList
    |> List.splitOn (fun line -> String.IsNullOrWhiteSpace(line))
    |> List.map (fun pair ->
        if List.length pair <> 2 then
            failwithf "Invalid input: %A" pair

        let left = parsePacketData pair[0]
        let right = parsePacketData pair[1]
        (left, right))

//////////////////////////
// Tests
//////////////////////////

let run_tests () =

    test <@ parsePacketData "[1,3,1]" = DataList [ Number 1; Number 3; Number 1 ] @>
    test <@ parsePacketData "[[1],2,3]" = DataList [ DataList [ Number 1 ]; Number 2; Number 3 ] @>
    test <@ parsePacketData "[[1],[2,3]]" = DataList [ DataList [ Number 1 ]; DataList [ Number 2; Number 3 ] ] @>

run_tests ()

//////////////////////////
// AoC
//////////////////////////
