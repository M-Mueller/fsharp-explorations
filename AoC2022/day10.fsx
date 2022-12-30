#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

type Instruction =
    | Noop
    | Addx of int


let parseInput filename : Instruction list =
    IO.File.ReadLines(filename)
    |> Seq.map (fun line ->
        match line.Split(" ") with
        | [| "noop" |] -> Noop
        | [| "addx"; amount |] -> Addx(int amount)
        | _ -> failwithf "Unsupported instruction: %s" line)
    |> Seq.toList


let executeInstruction (registerHistory: int list) (instruction: Instruction) : int list =
    let registerX = registerHistory[0]

    match instruction with
    | Noop -> registerX :: registerHistory
    | Addx amount -> [ registerX + amount; registerX ] @ registerHistory


let executeInstructions (instructions: Instruction list) : int list =
    instructions |> List.fold executeInstruction [ 1 ] |> List.rev


let signalStrength (registerHistory: int list) (cycle: int) : int = registerHistory[cycle - 1] * cycle


let sumOfInterestingSignalStrengths (registerHistory: int list) : int =
    let cycles = [ 20; 60; 100; 140; 180; 220 ]

    cycles |> List.map (signalStrength registerHistory) |> List.sum


let drawCrt (registerHistory: int list) : string =
    let width = 40
    let height = 6

    let rows = registerHistory |> List.chunkBySize 40 |> List.take 6

    let drawRow (row: int list) : string =
        row
        |> List.mapi (fun col spritePos -> if abs (spritePos - col) <= 1 then '#' else '.')
        |> List.toArray
        |> String

    rows |> List.map drawRow |> (fun l -> String.Join("\n", l))



//////////////////////////
// Tests
//////////////////////////

let tests =
    let instructions1 = parseInput "day10.test1.in"
    test <@ instructions1 = [ Noop; Addx 3; Addx -5 ] @>

    let result1 = executeInstructions instructions1
    test <@ result1 = [ 1; 1; 1; 4; 4; -1 ] @>

    let instructions2 = parseInput "day10.test2.in"
    let result2 = executeInstructions instructions2
    test <@ result2[19] = 21 @>
    test <@ result2[59] = 19 @>
    test <@ result2[99] = 18 @>
    test <@ result2[139] = 21 @>
    test <@ result2[179] = 16 @>
    test <@ result2[219] = 18 @>

    test <@ signalStrength result2 20 = 420 @>
    test <@ signalStrength result2 60 = 1140 @>
    test <@ signalStrength result2 220 = 3960 @>

    test <@ sumOfInterestingSignalStrengths result2 = 13140 @>

    let crt2 =
        """##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."""

    test <@ drawCrt result2 = crt2 @>

//////////////////////////
// AoC
//////////////////////////
let instructions = parseInput "day10.in"
let registerHistory = executeInstructions instructions

registerHistory
|> sumOfInterestingSignalStrengths
|> printfn "The sum of interesting signal strengths is %d"

registerHistory |> drawCrt |> printfn "%s"
