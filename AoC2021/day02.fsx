#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote

type Command =
    | Forward of int
    | Down of int
    | Up of int

type Position =
    { horizontal: int
      depth: int
      aim: int }

let parseCommands (lines: string seq) =
    lines
    |> Seq.map
        (fun line ->
            match line.Split(' ') with
            | [| "forward"; value |] -> Forward(int value)
            | [| "up"; value |] -> Up(int value)
            | [| "down"; value |] -> Down(int value)
            | _ -> failwithf "%s is not a supported command" line)

let execCommands (execCommand: Position -> Command -> Position) (commands: Command seq) =
    let initial = { horizontal = 0; depth = 0; aim = 0 }

    Seq.fold execCommand initial commands

let execCommandsWithAim =
    execCommands
        (fun pos command ->
            match command with
            | Forward units ->
                { pos with
                      horizontal = pos.horizontal + units
                      depth = pos.depth + pos.aim * units }
            | Down units -> { pos with aim = pos.aim + units }
            | Up units -> { pos with aim = pos.aim - units })

let execCommandsWithoutAim =
    execCommands
        (fun pos command ->
            match command with
            | Forward units ->
                { pos with
                      horizontal = pos.horizontal + units }
            | Down units -> { pos with depth = pos.depth + units }
            | Up units -> { pos with depth = pos.depth - units })


//////////////////////////
/// Tests
//////////////////////////

let ``test parseCommands`` () =
    let commands =
        [ "forward 5"
          "down 10"
          "up 8"
          "forward 2" ]
        |> List.toSeq

    let expected = [ Forward 5; Down 10; Up 8; Forward 2 ]

    test <@ parseCommands commands |> Seq.toList = expected @>

let ``test execCommands`` () =
    let commands =
        [ Forward 5
          Down 5
          Forward 8
          Up 3
          Down 8
          Forward 2 ]
        |> List.toSeq

    test <@ execCommandsWithoutAim commands = { horizontal = 15; depth = 10; aim = 0 } @>

    test
        <@ execCommandsWithAim commands = { horizontal = 15
                                            depth = 60
                                            aim = 10 } @>

``test parseCommands`` ()
``test execCommands`` ()


//////////////////////////
/// AoC Results
//////////////////////////

let commands =
    parseCommands (System.IO.File.ReadLines("day02.in"))

let positionWithoutAim = execCommandsWithoutAim commands
let positionWithAim = execCommandsWithAim commands

printfn
    "Final position without aim is: %A, multiplied: %d"
    positionWithoutAim
    (positionWithoutAim.horizontal
     * positionWithoutAim.depth)

printfn
    "Final position without aim is: %A, multiplied: %d"
    positionWithAim
    (positionWithAim.horizontal * positionWithAim.depth)
