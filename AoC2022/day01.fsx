#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

let parseInput (lines: string seq) : int [] [] =
    lines
    |> Seq.toArray
    |> Array.fold
        (fun (result: int [] [], current: int []) line ->
            if String.IsNullOrEmpty line then
                (Array.append result [| current |], [||])
            else
                (result, Array.append current [| (int line) |]))
        (Array.empty, Array.empty)
    |> fun (result, current) ->
        if Array.isEmpty current then
            result
        else
            Array.append result [| current |]


let maxCalories (elves: int [] []) =
    elves |> Array.map Array.sum |> Array.max

let top3Calories (elves: int [] []) =
    elves
    |> Array.map Array.sum
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum


//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let elves =
        parseInput (IO.File.ReadLines("day01.test.in"))

    test <@ maxCalories elves = 24000 @>
    test <@ top3Calories elves = 45000 @>


//////////////////////////
/// AoC
//////////////////////////

let elves =
    parseInput (IO.File.ReadLines("day01.in"))

elves
|> maxCalories
|> printfn "The elf with the most calories carries %d calories"

elves
|> top3Calories
|> printfn "The 3 elves with the most calories carry %d calories combined"
