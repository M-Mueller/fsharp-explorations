#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

type BingoBoard = { numbers: int [,]; marked: bool [,] }

let parseInput (lines: string seq) : int [] * BingoBoard seq =
    let parseNumbers (line: string) = line.Split(',') |> Array.map int

    let parseBoard (lines: string seq) : BingoBoard =
        if not (String.IsNullOrWhiteSpace(Seq.head lines)) then
            failwith "Boards must be separated by newlines"
        else
            lines
            |> Seq.tail
            |> Seq.map
                (fun line ->
                    line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map int)
            |> Seq.toArray
            |> array2D
            |> fun numbers ->
                { numbers = numbers
                  marked = Array2D.create (Array2D.length1 numbers) (Array2D.length2 numbers) false }

    let numbers = parseNumbers (Seq.head lines)

    let boards =
        lines
        |> Seq.tail
        |> Seq.chunkBySize 6
        |> Seq.map parseBoard

    (numbers, boards)


let updateBoard (number: int) (board: BingoBoard) : BingoBoard =
    { board with
          marked =
              board.marked
              |> Array2D.mapi
                  (fun y x marked ->
                      if marked || board.numbers.[y, x] = number then
                          true
                      else
                          false) }

let updateBoards (number: int) (boards: BingoBoard seq) : BingoBoard seq = Seq.map (updateBoard number) boards

let boardIsWon (board: BingoBoard) : bool =
    let wonColumn =
        seq { 0 .. Array2D.length2 board.marked - 1 }
        |> Seq.map (fun index -> board.marked.[*, index] |> Array.fold (&&) true)
        |> Seq.exists id

    let wonRow =
        seq { 0 .. Array2D.length1 board.marked - 1 }
        |> Seq.map (fun index -> board.marked.[index, *] |> Array.fold (&&) true)
        |> Seq.exists id

    wonColumn || wonRow

let rec playBingoUntilFirstWinner
    (numbers: int seq)
    (boards: BingoBoard seq)
    : BingoBoard seq * (BingoBoard * int) option =
    match Seq.tryHead numbers with
    | Some number ->
        let boards = updateBoards number boards

        match Seq.tryFind boardIsWon boards with
        | Some winner -> (boards, Some(winner, number))
        | None -> playBingoUntilFirstWinner (Seq.tail numbers) boards
    | None -> (boards, None)

let rec playBingoUntilLastWinner (numbers: int seq) (boards: BingoBoard seq) : (BingoBoard * int) =
    let number = Seq.head numbers
    let boards = updateBoards number boards
    let firstBoard = Seq.head boards

    if Seq.length boards = 1 && boardIsWon firstBoard then
        (firstBoard, number)
    else
        boards
        |> Seq.filter (boardIsWon >> not)
        |> playBingoUntilLastWinner (Seq.tail numbers)


let sumOfUnmarkedNumbers (board: BingoBoard) =
    let mutable sum = 0

    board.marked
    |> Array2D.iteri
        (fun y x marked ->
            if not marked then
                sum <- sum + board.numbers.[y, x])

    sum


//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let input = IO.File.ReadLines("day04.test.in")

    let expectedNumbers =
        [| 7
           4
           9
           5
           11
           17
           23
           2
           0
           14
           21
           24
           10
           16
           13
           6
           15
           25
           12
           22
           18
           20
           8
           19
           3
           26
           1 |]

    let board1 =
        { numbers =
              array2D [ [ 22; 13; 17; 11; 0 ]
                        [ 8; 2; 23; 4; 24 ]
                        [ 21; 9; 14; 16; 7 ]
                        [ 6; 10; 3; 18; 5 ]
                        [ 1; 12; 20; 15; 19 ] ]
          marked = Array2D.create 5 5 false }

    let board2 =
        { numbers =
              array2D [ [ 3; 15; 0; 2; 22 ]
                        [ 9; 18; 13; 17; 5 ]
                        [ 19; 8; 7; 25; 23 ]
                        [ 20; 11; 10; 24; 4 ]
                        [ 14; 21; 16; 12; 6 ] ]
          marked = Array2D.create 5 5 false }

    let board3 =
        { numbers =
              array2D [ [ 14; 21; 17; 24; 4 ]
                        [ 10; 16; 15; 9; 19 ]
                        [ 18; 8; 23; 26; 20 ]
                        [ 22; 11; 13; 6; 5 ]
                        [ 2; 0; 12; 3; 7 ] ]
          marked = Array2D.create 5 5 false }

    let expectedBoards = [ board1; board2; board3 ]

    let (numbers, boards) = parseInput input

    test <@ numbers = expectedNumbers @>
    test <@ boards |> Seq.toList = expectedBoards @>

    let board1After6 =
        { numbers = board1.numbers
          marked =
              array2D [ [ false; false; false; false; false ]
                        [ false; false; false; false; false ]
                        [ false; false; false; false; false ]
                        [ true; false; false; false; false ]
                        [ false; false; false; false; false ] ] }

    test <@ updateBoard 6 board1 = board1After6 @>

    let expected1After5 =
        { numbers = board1.numbers
          marked =
              array2D [ [ false; false; false; true; false ]
                        [ false; false; false; true; false ]
                        [ false; true; false; false; true ]
                        [ false; false; false; false; true ]
                        [ false; false; false; false; false ] ] }

    let expected2After5 =
        { numbers = board2.numbers
          marked =
              array2D [ [ false; false; false; false; false ]
                        [ true; false; false; false; true ]
                        [ false; false; true; false; false ]
                        [ false; true; false; false; true ]
                        [ false; false; false; false; false ] ] }

    let expected3After5 =
        { numbers = board3.numbers
          marked =
              array2D [ [ false; false; false; false; true ]
                        [ false; false; false; true; false ]
                        [ false; false; false; false; false ]
                        [ false; true; false; false; true ]
                        [ false; false; false; false; true ] ] }

    let (resultAfter5, winnerAfter5) =
        playBingoUntilFirstWinner [ 7; 4; 9; 5; 11 ] [
            board1
            board2
            board3
        ]

    test <@ Seq.length resultAfter5 = 3 @>
    test <@ Seq.item 0 resultAfter5 = expected1After5 @>
    test <@ Seq.item 1 resultAfter5 = expected2After5 @>
    test <@ Seq.item 2 resultAfter5 = expected3After5 @>

    test <@ winnerAfter5 = None @>

    let expected1AfterAll =
        { numbers = board1.numbers
          marked =
              array2D [ [ false; false; true; true; true ]
                        [ false; true; true; true; true ]
                        [ true; true; true; false; true ]
                        [ false; false; false; false; true ]
                        [ false; false; false; false; false ] ] }

    let expected2AfterAll =
        { numbers = board2.numbers
          marked =
              array2D [ [ false; false; true; true; false ]
                        [ true; false; false; true; true ]
                        [ false; false; true; false; true ]
                        [ false; true; false; true; true ]
                        [ true; true; false; false; false ] ] }

    let expected3AfterAll =
        { numbers = board3.numbers
          marked =
              array2D [ [ true; true; true; true; true ]
                        [ false; false; false; true; false ]
                        [ false; false; true; false; false ]
                        [ false; true; false; false; true ]
                        [ true; true; false; false; true ] ] }

    let (resultAfterAll, winnerAfterAll) =
        playBingoUntilFirstWinner expectedNumbers [ board1; board2; board3 ]

    test <@ Seq.length resultAfterAll = 3 @>
    test <@ Seq.item 0 resultAfterAll = expected1AfterAll @>
    test <@ Seq.item 1 resultAfterAll = expected2AfterAll @>
    test <@ Seq.item 2 resultAfterAll = expected3AfterAll @>

    test <@ winnerAfterAll = Some(expected3AfterAll, 24) @>

    test <@ sumOfUnmarkedNumbers expected3AfterAll = 188 @>

    let (loserBoardAfterAll, loserNumberAfterAll) =
        playBingoUntilLastWinner expectedNumbers [ board1; board2; board3 ]

    test <@ loserBoardAfterAll.numbers = board2.numbers @>
    test <@ loserNumberAfterAll = 13 @>

    test <@ sumOfUnmarkedNumbers loserBoardAfterAll = 148 @>


//////////////////////////
/// AoC Results
//////////////////////////

let (numbers, boards) =
    parseInput (IO.File.ReadLines("day04.in"))

let (_, winner) = playBingoUntilFirstWinner numbers boards

match winner with
| Some (board, number) -> printfn "Final winner score is %d" (sumOfUnmarkedNumbers board * number)
| None -> printfn "No winner"

let (loserBoard, loserNumber) = playBingoUntilLastWinner numbers boards

printfn "Final loser score is %d" (sumOfUnmarkedNumbers loserBoard * loserNumber)
