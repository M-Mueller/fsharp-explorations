#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System


let requiredFuel (values: int seq) (position: int) =
    values
    |> Seq.map (fun v -> abs (v - position))
    |> Seq.sum


let minFuelPos (values: int seq) =
    assert ((Seq.length values) % 2 = 0)
    let sorted = Seq.sort values

    let mid = Seq.length values / 2

    Seq.item mid sorted


let requiredFuel2 (values: int seq) (position: int) =
    values
    |> Seq.map (fun v -> abs (v - position))
    |> Seq.map (fun n -> n * (n + 1) / 2)
    |> Seq.sum


let minFuelPos2 (values: int seq) =
    seq { Seq.min values .. Seq.max values }
    |> Seq.map (fun pos -> (pos, requiredFuel2 values pos))
    |> Seq.minBy snd
    |> fst


//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let testCrabs = [| 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 |]

    test <@ minFuelPos testCrabs = 2 @>
    test <@ requiredFuel testCrabs 2 = 37 @>

    test <@ minFuelPos2 testCrabs = 5 @>
    test <@ requiredFuel2 testCrabs 5 = 168 @>

//////////////////////////
/// AoC
//////////////////////////

let crabs =
    IO.File.ReadLines("day07.in")
    |> Seq.head
    |> (fun line -> line.Split(","))
    |> Array.map int

crabs
|> minFuelPos
|> requiredFuel crabs
|> printfn "Part 1: Required fuel to cheapest position is %d"

crabs
|> minFuelPos2
|> requiredFuel2 crabs
|> printfn "Part 2: Required fuel to cheapest position is %d"
