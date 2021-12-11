#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System


let parseInput (lines: string seq) : int [,] =
    let charToInt (c: char) = int c - int '0'

    let lineToArray (line: string) : int array =
        line.ToCharArray() |> Array.map charToInt

    lines |> Seq.map lineToArray |> array2D


let drawArray2D (toString: 'T -> string) (heightmap: 'T [,]) : string =
    let rows =
        seq { for row in 0 .. Array2D.length1 heightmap - 1 -> heightmap.[row, *] }

    let rowToString = Array.map toString >> String.concat ""

    rows |> Seq.map rowToString |> String.concat "\n"


let drawHeightmap = drawArray2D string


let drawFilteredHeightmap (heightmap: 'T option [,]) =
    drawArray2D (Option.map string >> Option.defaultValue ".") heightmap


let drawBasin (heightmap: int [,]) (basin: Set<int * int>) =
    heightmap
    |> Array2D.mapi
        (fun y x v ->
            if Set.contains (y, x) basin then
                Some v
            else
                None)
    |> drawFilteredHeightmap


let filterLowPoints (heightmap: int [,]) : int option [,] =
    let rows = Array2D.length1 heightmap
    let cols = Array2D.length2 heightmap

    let neighbors y x =
        [ if y > 0 then heightmap.[y - 1, x]
          if y < rows - 1 then
              heightmap.[y + 1, x]
          if x > 0 then heightmap.[y, x - 1]
          if x < cols - 1 then
              heightmap.[y, x + 1] ]

    heightmap
    |> Array2D.mapi
        (fun y x v ->
            if v < (neighbors y x |> List.min) then
                Some v
            else
                None)


let computeRiskLevel (heightmap: int option [,]) : int =
    heightmap
    |> Seq.cast<int option>
    |> Seq.choose id
    |> Seq.map ((+) 1)
    |> Seq.sum


let findBasins (heightmap: int [,]) : (Set<int * int> seq) =
    let rows = Array2D.length1 heightmap
    let cols = Array2D.length2 heightmap

    let lowPoints = filterLowPoints heightmap

    let lowPointCoords =
        lowPoints
        |> Array2D.mapi
            (fun y x v ->
                if Option.isSome v then
                    Some(y, x)
                else
                    None)
        |> Seq.cast<(int * int) option>
        |> Seq.choose id

    let neighbors y x =
        [ if y > 0 then (y - 1, x)
          if y < rows - 1 then (y + 1, x)
          if x > 0 then (y, x - 1)
          if x < cols - 1 then (y, x + 1) ]

    let rec floodFill (basin: Set<int * int>) =
        let newBasin =
            basin
            |> Set.toSeq
            |> Seq.map
                (fun (y, x) ->
                    neighbors y x
                    |> Seq.filter (fun (y, x) -> heightmap.[y, x] < 9)
                    |> Set
                    |> Set.add (y, x))
            |> Set.unionMany

        if newBasin = basin then
            newBasin
        else
            floodFill newBasin

    lowPointCoords
    |> Seq.filter (fun (y, x) -> heightmap.[y, x] < 9)
    |> Seq.map (fun (y, x) -> floodFill (Set [ y, x ]))


let largestBasinSize (basins : Set<int * int> seq) =
    basins
    |> Seq.map Set.count
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (*) 1


//////////////////////////
/// Tests
//////////////////////////

let runTest =
    let sample =
        IO.File.ReadLines("day09.test.in") |> parseInput

    test <@ Array2D.length1 sample = 5 @>
    test <@ Array2D.length2 sample = 10 @>

    let expectedHeightmap = "2199943210\n\
                             3987894921\n\
                             9856789892\n\
                             8767896789\n\
                             9899965678"

    test <@ drawHeightmap sample = expectedHeightmap @>
    test <@ sample.[0, 0] = 2 @>
    test <@ sample.[0, 9] = 0 @>
    test <@ sample.[4, 0] = 9 @>
    test <@ sample.[4, 9] = 8 @>
    test <@ sample.[2, 4] = 7 @>

    let expectedLowPoints = ".1.......0\n\
                             ..........\n\
                             ..5.......\n\
                             ..........\n\
                             ......5..."

    let lowPoints = filterLowPoints sample

    test <@ drawFilteredHeightmap lowPoints = expectedLowPoints @>

    test <@ computeRiskLevel lowPoints = 15 @>

    let basins = findBasins sample

    test <@ Seq.length basins = 4 @>

    let expectedBasin0 = "21........\n\
                          3.........\n\
                          ..........\n\
                          ..........\n\
                          .........."

    test <@ drawBasin sample (Seq.item 0 basins) = expectedBasin0 @>

    test <@ largestBasinSize basins = 1134 @>


//////////////////////////
/// AoC
//////////////////////////

IO.File.ReadLines("day09.in")
|> parseInput
|> filterLowPoints
|> computeRiskLevel
|> printfn "Risk level is %d"

IO.File.ReadLines("day09.in")
|> parseInput
|> findBasins
|> largestBasinSize
|> printfn "3 largest basin multiplied is %d"
