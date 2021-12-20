#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

let parseInput (lines: string seq) : int [,] =
    let charToInt (c: char) = int c - int '0'

    let lineToArray (line: string) : int array =
        line.ToCharArray() |> Array.map charToInt

    lines |> Seq.map lineToArray |> array2D


let increaseMapSize (initial: int [,]) =
    let length1 = Array2D.length1 initial
    let length2 = Array2D.length2 initial
    let newLength1 = length1 * 5
    let newLength2 = length2 * 5

    let map = Array2D.zeroCreate newLength1 newLength2

    Array2D.mapi
        (fun y x _ ->
            let tileY = y / length1
            let tileX = x / length2

            let value = initial.[y % length1, x % length2]
            let value = value + tileX + tileY
            if value > 9 then value - 9 else value)
        map


let unvisitedNeighbors (visited: bool [,]) ((y, x): (int * int)) =
    let rows = Array2D.length1 visited
    let cols = Array2D.length2 visited

    seq {
        if y > 0 then (y - 1, x)
        if y < rows - 1 then (y + 1, x)
        if x > 0 then (y, x - 1)
        if x < cols - 1 then (y, x + 1)
    }
    |> Seq.filter (fun (y, x) -> not visited.[y, x])


let lowestRisk (map: int [,]) =
    let height = Array2D.length1 map
    let width = Array2D.length2 map

    let visited : bool [,] =
        Array2D.init height width (fun _ _ -> false)

    let distances : int [,] =
        Array2D.init height width (fun _ _ -> Int32.MaxValue)

    distances.[0, 0] <- 0

    let rec processNode ((y, x): (int * int)) =
        let neighbors = unvisitedNeighbors visited (y, x)

        for (ny, nx) in neighbors do
            let distance = distances.[y, x] + map.[ny, nx]

            if distance < distances.[ny, nx] then
                distances.[ny, nx] <- distance

        visited.[y, x] <- true

        let mutable nextNode = (0, 0)
        let mutable smallestDist = Int32.MaxValue

        Array2D.iteri
            (fun y x d ->
                if not visited.[y, x]
                   && distances.[y, x] < smallestDist then
                    nextNode <- (y, x)
                    smallestDist <- distances.[y, x])
            distances

        if visited.[height - 1, width - 1] then
            distances.[height - 1, width - 1]
        else
            processNode nextNode

    processNode (0, 0)


//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let sample =
        [ "1163751742"
          "1381373672"
          "2136511328"
          "3694931569"
          "7463417111"
          "1319128137"
          "1359912421"
          "3125421639"
          "1293138521"
          "2311944581" ]
        |> parseInput

    test <@ Array2D.length1 sample = 10 @>
    test <@ Array2D.length2 sample = 10 @>
    test <@ sample.[0, 0] = 1 @>
    test <@ sample.[5, 0] = 1 @>
    test <@ sample.[0, 5] = 5 @>

    test <@ lowestRisk sample = 40 @>

    let sampleLarge = increaseMapSize sample

    test <@ Array2D.length1 sampleLarge = 50 @>
    test <@ Array2D.length2 sampleLarge = 50 @>
    test <@ sampleLarge.[0, 0] = 1 @>
    test <@ sampleLarge.[5, 0] = 1 @>
    test <@ sampleLarge.[13, 0] = 4 @>
    test <@ sampleLarge.[0, 5] = 5 @>
    test <@ sampleLarge.[0, 14] = 8 @>

    test <@ lowestRisk sampleLarge = 315 @>


//////////////////////////
/// AoC
//////////////////////////

IO.File.ReadLines("day15.in")
|> parseInput
|> lowestRisk
|> printfn "The lowest possible risk is %d"

IO.File.ReadLines("day15.in")
|> parseInput
|> increaseMapSize
|> lowestRisk
|> printfn "The lowest possible risk on the complete map is %d"
