#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

type Fold =
    | FoldUp of int
    | FoldLeft of int


let parseInput (lines: string seq) : (int [,] * Fold list) =
    let parseDot (line: string) : (int * int) =
        match line.Split(",") with
        | [| x; y |] -> (int y, int x)
        | _ -> failwithf "Invalid dot: %s" line

    let parseFold (line: string) =
        match line.Split("=") with
        | [| "fold along y"; y |] -> FoldUp(int y)
        | [| "fold along x"; x |] -> FoldLeft(int x)
        | _ -> failwithf "Invalid fold: %s" line

    let dots, folds, _ =
        Seq.fold
            (fun (dots, folds, parseDots) line ->
                if String.IsNullOrEmpty line then
                    (dots, folds, false)
                else if parseDots then
                    ((parseDot line) :: dots, folds, true)
                else
                    (dots, (parseFold line) :: folds, false))
            ([], [], true)
            lines

    let createArray (dots: (int * int) list) =
        let length1 = (dots |> List.map fst |> List.max) + 1
        let length2 = (dots |> List.map snd |> List.max) + 1

        let paper = Array2D.zeroCreate length1 length2

        for (y, x) in dots do
            paper.[y, x] <- 1

        paper

    ((createArray dots), List.rev folds)


let drawPaper (paper: int [,]) =
    let rows =
        seq { for row in 0 .. Array2D.length1 paper - 1 -> paper.[row, *] }

    rows
    |> Seq.map (Seq.map (fun v -> if v > 0 then "#" else "."))
    |> Seq.map (String.concat "")
    |> String.concat "\n"


let splitPaperHorizontal (y: int) (paper: int [,]) =
    (paper.[0..y - 1, 0..], paper.[y + 1.., 0..])


let splitPaperVertical (x: int) (paper: int [,]) =
    (paper.[0.., 0..x - 1], paper.[0.., x + 1..])


let foldPaper paper fold =
    match fold with
    | FoldUp y ->
        let top, bottom = splitPaperHorizontal y paper

        let topHeight = Array2D.length1 top

        Array2D.iteri
            (fun y x v ->
                if v > 0 then
                    top.[topHeight - 1 - y, x] <- v)
            bottom

        top
    | FoldLeft x ->
        let left, right = splitPaperVertical x paper

        let leftWidth = Array2D.length2 left

        Array2D.iteri
            (fun y x v ->
                if v > 0 then
                    left.[y, leftWidth - 1 - x] <- v)
            right

        left


let countDots paper =
    let mutable numDots = 0
    Array2D.iter (fun dot -> if dot > 0 then numDots <- numDots + 1) paper
    numDots


//////////////////////////
/// Tests
//////////////////////////

let runTest =
    let paper, folds =
        IO.File.ReadLines "day13.test.in" |> parseInput

    let expectedPaper =
        [ "...#..#..#."
          "....#......"
          "..........."
          "#.........."
          "...#....#.#"
          "..........."
          "..........."
          "..........."
          "..........."
          "..........."
          ".#....#.##."
          "....#......"
          "......#...#"
          "#.........."
          "#.#........" ]
        |> String.concat "\n"

    test <@ drawPaper paper = expectedPaper @>
    test <@ folds = [ FoldUp 7; FoldLeft 5 ] @>

    let expectedHorSplitTop =
        [ "...#..#..#."; "....#......"; "..........."; "#.........."; "...#....#.#"; "..........."; "..........." ]
        |> String.concat "\n"

    let expectedHorSplitBottom =
        [ "..........."; "..........."; ".#....#.##."; "....#......"; "......#...#"; "#.........."; "#.#........" ]
        |> String.concat "\n"

    test <@ splitPaperHorizontal 7 paper |> fst |> drawPaper = expectedHorSplitTop @>
    test <@ splitPaperHorizontal 7 paper |> snd |> drawPaper = expectedHorSplitBottom @>

    let expectedPaperAfterFoldUp =
        [ "#.##..#..#."; "#...#......"; "......#...#"; "#...#......"; ".#.#..#.###"; "..........."; "..........." ]
        |> String.concat "\n"

    let paperAfterFoldUp = foldPaper paper (FoldUp 7)

    test <@ paperAfterFoldUp |> drawPaper = expectedPaperAfterFoldUp @>
    test <@ countDots paperAfterFoldUp = 17 @>

    let expectedVerSplitLeft =
        [ "#.##."; "#...#"; "....."; "#...#"; ".#.#."; "....."; "....." ]
        |> String.concat "\n"

    let expectedVerSplitRight =
        [ "#..#."; "....."; "#...#"; "....."; "#.###"; "....."; "....." ]
        |> String.concat "\n"

    test
        <@ splitPaperVertical 5 paperAfterFoldUp
           |> fst
           |> drawPaper = expectedVerSplitLeft @>

    test
        <@ splitPaperVertical 5 paperAfterFoldUp
           |> snd
           |> drawPaper = expectedVerSplitRight @>

    let expectedPaperAfterFoldLeft =
        [ "#####"; "#...#"; "#...#"; "#...#"; "#####"; "....."; "....." ]
        |> String.concat "\n"

    let paperAfterFoldLeft = foldPaper paperAfterFoldUp (FoldLeft 5)

    test <@ paperAfterFoldLeft |> drawPaper = expectedPaperAfterFoldLeft @>
    test <@ countDots paperAfterFoldLeft = 16 @>

    test <@ List.fold foldPaper paper folds |> drawPaper = expectedPaperAfterFoldLeft @>


//////////////////////////
/// AoC
//////////////////////////

IO.File.ReadLines("day13.in")
|> parseInput
|> fun (paper, folds) -> foldPaper paper (List.head folds)
|> countDots
|> printfn "There are %d dots after the first fold"


IO.File.ReadLines("day13.in")
|> parseInput
||> List.fold foldPaper
|> drawPaper
|> printfn "%s"
