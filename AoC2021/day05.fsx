#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

type Line = { start: int * int; end_: int * int }


module Line =
    let isHorizontal (line: Line) = snd line.start = snd line.end_
    let isVertical (line: Line) = fst line.start = fst line.end_


let parseInput (lines: string seq) : Line seq =
    let parsePoint (point: string) : int * int =
        match point.Split(",") with
        | [| x; y |] -> (int x, int y)
        | _ -> failwithf "Not a valid point: %s" point

    let parseLine (line: string) : Line =
        match line.Split("->") with
        | [| a; b |] ->
            { start = parsePoint a
              end_ = parsePoint b }
        | _ -> failwithf "Not a valid line: %s" line

    Seq.map parseLine lines


let plotDiagram (lines: Line seq) =
    let width =
        lines
        |> Seq.map (fun line -> max (fst line.start) (fst line.end_))
        |> Seq.max
        |> (+) 1

    let height =
        lines
        |> Seq.map (fun line -> max (snd line.start) (snd line.end_))
        |> Seq.max
        |> (+) 1

    let mutable diagram = Array2D.create height width 0

    for line in lines do
        if Line.isHorizontal line then
            let x0 = min (fst line.start) (fst line.end_)
            let x1 = max (fst line.start) (fst line.end_)
            let y = snd line.start

            for x in x0 .. x1 do
                diagram.[y, x] <- diagram.[y, x] + 1
        elif Line.isVertical line then
            let x = fst line.start
            let y0 = min (snd line.start) (snd line.end_)
            let y1 = max (snd line.start) (snd line.end_)

            for y in y0 .. y1 do
                diagram.[y, x] <- diagram.[y, x] + 1
        else
            let x0 = (fst line.start)
            let x1 = (fst line.end_)
            let y0 = (snd line.start)
            let y1 = (snd line.end_)
            let xd = if x0 < x1 then 1 else -1
            let yd = if y0 < y1 then 1 else -1

            for (x, y) in Seq.zip (seq { x0 .. xd .. x1 }) (seq { y0 .. yd .. y1 }) do
                diagram.[y, x] <- diagram.[y, x] + 1

    diagram


let numPointsWithOverlap (minOverlap: int) (diagram: int [,]) =
    let mutable numOverlaps = 0

    Array2D.iter
        (fun overlaps ->
            if overlaps >= minOverlap then
                numOverlaps <- numOverlaps + 1)
        diagram

    numOverlaps

//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let expectedLines =
        [ { start = (0, 9); end_ = (5, 9) }
          { start = (8, 0); end_ = (0, 8) }
          { start = (9, 4); end_ = (3, 4) }
          { start = (2, 2); end_ = (2, 1) }
          { start = (7, 0); end_ = (7, 4) }
          { start = (6, 4); end_ = (2, 0) }
          { start = (0, 9); end_ = (2, 9) }
          { start = (3, 4); end_ = (1, 4) }
          { start = (0, 0); end_ = (8, 8) }
          { start = (5, 5); end_ = (8, 2) } ]

    let actualLines =
        parseInput (IO.File.ReadLines("day05.test.in"))

    test <@ actualLines |> Seq.toList = expectedLines @>

    let horOrVerLines =
        Seq.filter (fun line -> Line.isHorizontal line || Line.isVertical line) actualLines

    let expectedDiagram =
        array2D [ [ 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 ]
                  [ 0; 0; 1; 0; 0; 0; 0; 1; 0; 0 ]
                  [ 0; 0; 1; 0; 0; 0; 0; 1; 0; 0 ]
                  [ 0; 0; 0; 0; 0; 0; 0; 1; 0; 0 ]
                  [ 0; 1; 1; 2; 1; 1; 1; 2; 1; 1 ]
                  [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                  [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                  [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                  [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
                  [ 2; 2; 2; 1; 1; 1; 0; 0; 0; 0 ] ]

    let actualDiagram = plotDiagram horOrVerLines
    test <@ actualDiagram = expectedDiagram @>

    test <@ numPointsWithOverlap 2 actualDiagram = 5 @>

    let expectedDiagramWithDiags =
        array2D [ [ 1; 0; 1; 0; 0; 0; 0; 1; 1; 0 ]
                  [ 0; 1; 1; 1; 0; 0; 0; 2; 0; 0 ]
                  [ 0; 0; 2; 0; 1; 0; 1; 1; 1; 0 ]
                  [ 0; 0; 0; 1; 0; 2; 0; 2; 0; 0 ]
                  [ 0; 1; 1; 2; 3; 1; 3; 2; 1; 1 ]
                  [ 0; 0; 0; 1; 0; 2; 0; 0; 0; 0 ]
                  [ 0; 0; 1; 0; 0; 0; 1; 0; 0; 0 ]
                  [ 0; 1; 0; 0; 0; 0; 0; 1; 0; 0 ]
                  [ 1; 0; 0; 0; 0; 0; 0; 0; 1; 0 ]
                  [ 2; 2; 2; 1; 1; 1; 0; 0; 0; 0 ] ]

    let actualDiagramWithDiags = plotDiagram expectedLines
    test <@ actualDiagramWithDiags = expectedDiagramWithDiags @>

    test <@ numPointsWithOverlap 2 actualDiagramWithDiags = 12 @>


//////////////////////////
/// AoC
//////////////////////////

let lines =
    parseInput (IO.File.ReadLines("day05.in"))


lines
|> Seq.filter (fun line -> Line.isHorizontal line || Line.isVertical line)
|> plotDiagram
|> numPointsWithOverlap 2
|> printfn "Two lines overlap at least 2 times in %d points"


lines
|> plotDiagram
|> numPointsWithOverlap 2
|> printfn "Two lines overlap at least 2 times in %d points (with diagonals)"
