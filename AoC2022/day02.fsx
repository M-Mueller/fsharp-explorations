#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

type Shape =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Player1Win
    | Player2Win
    | Draw

let parseIntoTokens (lines: string seq) : (string * string) list =
    lines
    |> Seq.toList
    |> List.map (fun s -> s.Split(" "))
    |> List.map
        (fun l ->
            match l with
            | [| a; b |] -> (a, b)
            | _ -> failwith "Invalid format")


let parseInput (lines: string seq) : (Shape * Shape) list =
    lines
    |> parseIntoTokens
    |> List.map
        (fun (a, b) ->
            let player1 =
                match a with
                | "A" -> Rock
                | "B" -> Paper
                | "C" -> Scissors
                | _ -> failwithf "Invalid shape: %s" a

            let player2 =
                match b with
                | "X" -> Rock
                | "Y" -> Paper
                | "Z" -> Scissors
                | _ -> failwithf "Invalid shape: %s" b

            (player1, player2))


let parseInput2 (lines: string seq) : (Shape * Outcome) list =
    lines
    |> parseIntoTokens
    |> List.map
        (fun (a, b) ->
            let player1 =
                match a with
                | "A" -> Rock
                | "B" -> Paper
                | "C" -> Scissors
                | _ -> failwithf "Invalid shape: %s" a

            let outcome =
                match b with
                | "X" -> Player1Win
                | "Y" -> Draw
                | "Z" -> Player2Win
                | _ -> failwithf "Invalid outcome: %s" b

            (player1, outcome))


let play (player1: Shape) (player2: Shape) : Outcome =
    if player1 = player2 then
        Draw
    else
        match (player1, player2) with
        | (Rock, Scissors) -> Player1Win
        | (Paper, Rock) -> Player1Win
        | (Scissors, Paper) -> Player1Win
        | _ -> Player2Win


let pickShape (player1: Shape) (outcome: Outcome) : Shape =
    match outcome with
    | Player1Win ->
        match player1 with
        | Rock -> Scissors
        | Paper -> Rock
        | Scissors -> Paper
    | Player2Win ->
        match player1 with
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock
    | Draw -> player1


let pickShapes games =
    List.map (fun (player1, outcome) -> player1, (pickShape player1 outcome)) games


let score (player1: Shape) (player2: Shape) : int =
    let fromShape =
        match player2 with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let fromOutcome =
        match (play player1 player2) with
        | Player1Win -> 0
        | Player2Win -> 6
        | Draw -> 3

    fromShape + fromOutcome

let totalScore games =
    games
    |> List.map (fun (p1, p2) -> score p1 p2)
    |> List.sum

//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let games1 =
        parseInput (IO.File.ReadLines("day02.test.in"))

    let games2 =
        parseInput2 (IO.File.ReadLines("day02.test.in"))

    test <@ games1 = [ (Rock, Paper); (Paper, Rock); (Scissors, Scissors) ] @>
    test <@ games2 = [ (Rock, Draw); (Paper, Player1Win); (Scissors, Player2Win) ] @>

    test <@ play Rock Rock = Draw @>
    test <@ play Rock Paper = Player2Win @>
    test <@ play Rock Scissors = Player1Win @>
    test <@ play Paper Scissors = Player2Win @>

    test <@ pickShape Rock Draw = Rock @>
    test <@ pickShape Paper Player1Win = Rock @>
    test <@ pickShape Scissors Player2Win = Rock @>

    test <@ score Rock Paper = 8 @>
    test <@ score Paper Rock = 1 @>

    test <@ totalScore games1 = 15 @>
    test <@ games2 |> pickShapes |> totalScore = 12 @>


//////////////////////////
/// AoC
//////////////////////////
IO.File.ReadLines("day02.in")
|> parseInput
|> totalScore
|> printfn "Total score is %d"

IO.File.ReadLines("day02.in")
|> parseInput2
|> pickShapes
|> totalScore
|> printfn "Total score is %d"
