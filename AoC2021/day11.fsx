#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System


let parseInput (lines: string seq) : int [,] =
    let charToInt (c: char) = int c - int '0'

    let lineToArray (line: string) : int array =
        line.ToCharArray() |> Array.map charToInt

    lines |> Seq.map lineToArray |> array2D


let neighbors grid y x =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    seq {
        if x > 0 then
            (y, x - 1)
            if y > 0 then (y - 1, x - 1)
            if y < rows - 1 then (y + 1, x - 1)

        if y > 0 then (y - 1, x)
        if y < rows - 1 then (y + 1, x)

        if x < cols - 1 then
            (y, x + 1)
            if y > 0 then (y - 1, x + 1)
            if y < rows - 1 then (y + 1, x + 1)
    }


let rec _runSimulation (initialNumFlashes: int) (steps: int) (initial: int [,]) =
    let grid = Array2D.map ((+) 1) initial

    let mutable flashing = Set.empty<int * int>
    let mutable numFlashes = initialNumFlashes

    Array2D.iteri
        (fun y x v ->
            if v > 9 then
                flashing <- Set.add (y, x) flashing)
        grid

    while Set.count flashing > 0 do
        let (y, x) = Seq.head flashing
        flashing <- Set.remove (y, x) flashing

        grid.[y, x] <- 0
        numFlashes <- numFlashes + 1

        for (ny, nx) in neighbors grid y x do
            if grid.[ny, nx] > 0 then
                grid.[ny, nx] <- grid.[ny, nx] + 1

                if grid.[ny, nx] > 9 then
                    flashing <- Set.add (ny, nx) flashing

    if steps = 1 then
        (grid, numFlashes)
    else
        _runSimulation numFlashes (steps - 1) grid


let runSimulation = _runSimulation 0


let simulateUntilAllZero (initial: int [,]) =
    let zeros : int [,] =
        Array2D.zeroCreate (Array2D.length1 initial) (Array2D.length2 initial)

    let rec simulate step grid =
        let newGrid = runSimulation 1 grid |> fst

        if newGrid = zeros then
            step
        else
            simulate (step + 1) newGrid

    simulate 1 initial


//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let smallStep0 =
        "11111\n\
         19991\n\
         19191\n\
         19991\n\
         11111"
            .Split()
        |> parseInput

    let smallStep1 =
        "34543\n\
         40004\n\
         50005\n\
         40004\n\
         34543"
            .Split()
        |> parseInput

    let smallStep2 =
        "45654\n\
         51115\n\
         61116\n\
         51115\n\
         45654"
            .Split()
        |> parseInput

    test <@ smallStep0 |> runSimulation 1 |> fst = smallStep1 @>
    test <@ smallStep0 |> runSimulation 2 |> fst = smallStep2 @>

    let samples =
        IO.File.ReadLines("day11.test.in")
        |> Seq.filter (fun s -> not (String.IsNullOrWhiteSpace s))
        |> Seq.chunkBySize 10
        |> Seq.map parseInput

    test <@ Seq.length samples = 20 @>

    let initial = Seq.head samples
    let samples = Seq.tail samples

    let samplesSteps =
        List.concat [ [ 1 .. 10 ]
                      [ 20 .. 10 .. 100 ] ]

    test <@ Seq.length samplesSteps = Seq.length samples @>

    for sample, steps in Seq.zip samples samplesSteps do
        let grid = runSimulation steps initial |> fst
        test <@ grid = sample @>

    test <@ runSimulation 10 initial |> snd = 204 @>
    test <@ runSimulation 100 initial |> snd = 1656 @>

    test <@ simulateUntilAllZero initial = 195 @>


//////////////////////////
/// AoC
//////////////////////////

IO.File.ReadLines("day11.in")
|> parseInput
|> runSimulation 100
|> snd
|> printfn "There where %d flashes after 100 steps"


IO.File.ReadLines("day11.in")
|> parseInput
|> simulateUntilAllZero
|> printfn "It takes %d steps for all to flash"
