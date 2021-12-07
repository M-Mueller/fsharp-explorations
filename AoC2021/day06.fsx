#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System


let parseInput (lines: string seq) : int array =
    lines
    |> Seq.head
    |> (fun s -> s.Split(','))
    |> Array.map int


let rec evolvePopulation (days: int) (population: int array) =
    if (days = 0) then
        population
    else
        let (ready, waiting) =
            Array.partition (fun age -> age = 0) population

        let newPopulation =
            Array.concat [ Array.replicate ready.Length 8
                           Array.replicate ready.Length 6
                           Array.map (fun x -> x - 1) waiting ]

        evolvePopulation (days - 1) newPopulation


let evolvePopulation2 (days: int) (population: int array) =
    let maxFishAge = max 8 (Array.max population)
    let fishByAge : uint64 array = Array.zeroCreate (maxFishAge + 1)

    for fish in population do
        fishByAge.[fish] <- fishByAge.[fish] + 1UL

    for day in 1 .. days do
        let numNewFish = fishByAge.[0]

        for i in 0 .. (maxFishAge - 1) do
            fishByAge.[i] <- fishByAge.[i + 1]
        fishByAge.[maxFishAge] <- 0UL

        fishByAge.[6] <- fishByAge.[6] + numNewFish
        fishByAge.[8] <- fishByAge.[8] + numNewFish

    Array.sum fishByAge


//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let expectedInput = [| 3; 4; 3; 1; 2 |]

    let input =
        IO.File.ReadLines("day06.test.in") |> parseInput

    test <@ input = expectedInput @>

    let after1 = [| 2; 3; 2; 0; 1 |] |> Array.sort

    let after2 = [| 1; 2; 1; 6; 0; 8 |] |> Array.sort

    let after18 =
        [| 6; 0; 6; 4; 5; 6; 0; 1; 1; 2; 6; 0; 1; 1; 1; 2; 2; 3; 3; 4; 6; 7; 8; 8; 8; 8 |]
        |> Array.sort

    test <@ input |> evolvePopulation 1 |> Array.sort = after1 @>
    test <@ input |> evolvePopulation 2 |> Array.sort = after2 @>
    test <@ input |> evolvePopulation 18 |> Array.sort = after18 @>

    test <@ input |> evolvePopulation 18 |> Array.length = 26 @>
    test <@ input |> evolvePopulation 80 |> Array.length = 5934 @>
    test <@ input |> evolvePopulation2 18 = 26UL @>
    test <@ input |> evolvePopulation2 80 = 5934UL @>

//////////////////////////
/// AoC
//////////////////////////

let initialPopulation =
    IO.File.ReadLines("day06.in") |> parseInput

initialPopulation
|> evolvePopulation 80
|> Array.length
|> printfn "There are %d lanternfish after 80 days"

initialPopulation
|> evolvePopulation2 256
|> printfn "There are %d lanternfish after 256 days"

