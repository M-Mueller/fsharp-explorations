#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System
open System.Text.RegularExpressions


type Instruction =
    { source: int
      destination: int
      amount: int }


type Stack = char list


let parseStack (lines: string list) : Stack[] =
    let lines = List.rev lines

    let numStacks =
        (List.head lines).Split(" ", StringSplitOptions.RemoveEmptyEntries).Length

    let stacks = Array.init numStacks (fun _ -> [])
    let stackLines = lines |> List.tail |> List.toArray

    for i in 0 .. numStacks - 1 do
        for line in stackLines do
            let crate = line[i * 4 + 1]

            if crate <> ' ' then
                stacks[i] <- crate :: stacks[i]

    stacks


let parseInstructions (lines: string list) : Instruction list =
    let regex = Regex(@"move (\d+) from (\d+) to (\d+)", RegexOptions.Compiled)

    let parseLine (line: string) : Instruction =
        let m = regex.Match(line)

        if m.Success then
            assert (m.Groups.Count = 4)

            { source = int m.Groups[2].Value
              destination = int m.Groups[3].Value
              amount = int m.Groups[1].Value }
        else
            failwithf "Invalid input: line doesn't match pattern %s" line

    List.map parseLine lines


let parseInput (lines: string seq) : Stack[] * Instruction list =
    let lines = Seq.toList lines
    let separator = List.findIndex String.IsNullOrWhiteSpace lines
    let (stackLines, instructionLines) = List.splitAt separator lines
    let stacks = parseStack stackLines
    let instructions = instructionLines |> List.tail |> parseInstructions

    stacks, instructions


let applyInstruction (moveSingleCrate: bool) (stacks: Stack[]) (instruction: Instruction) =
    let source = stacks[instruction.source - 1]
    let destination = stacks[instruction.destination - 1]

    let splitPos = min instruction.amount source.Length

    let crates, source = List.splitAt splitPos source

    let crates = if moveSingleCrate then List.rev crates else crates

    let destination = List.append crates destination

    stacks
    |> Array.updateAt (instruction.source - 1) source
    |> Array.updateAt (instruction.destination - 1) destination


let applyInstructions moveSingleCrate stacks instructions =
    List.fold (applyInstruction moveSingleCrate) stacks instructions


let topOfStacks (stacks: Stack[]) : string = stacks |> Array.map List.head |> String

//////////////////////////
// Tests
//////////////////////////

let runTests =
    let stacks, instructions = parseInput (IO.File.ReadLines("day05.test.in"))

    test
        <@
            List.item 0 instructions = { source = 2
                                         destination = 1
                                         amount = 1 }
        @>

    test
        <@
            List.item 1 instructions = { source = 1
                                         destination = 3
                                         amount = 3 }
        @>

    test <@ stacks[0] = [ 'N'; 'Z' ] @>
    test <@ stacks[1] = [ 'D'; 'C'; 'M' ] @>
    test <@ stacks[2] = [ 'P' ] @>

    test <@ applyInstruction true stacks (List.item 0 instructions) = [| [ 'D'; 'N'; 'Z' ]; [ 'C'; 'M' ]; [ 'P' ] |] @>
    test <@ applyInstruction false stacks (List.item 0 instructions) = [| [ 'D'; 'N'; 'Z' ]; [ 'C'; 'M' ]; [ 'P' ] |] @>

    test <@ applyInstructions true stacks instructions |> topOfStacks = "CMZ" @>
    test <@ applyInstructions false stacks instructions |> topOfStacks = "MCD" @>


//////////////////////////
// AoC
//////////////////////////
let stacks, instructions = parseInput (IO.File.ReadLines("day05.in"))

applyInstructions true stacks instructions
|> topOfStacks
|> printfn "After rearrangment with CrateMover 9000, the top of the stack is %s"

applyInstructions false stacks instructions
|> topOfStacks
|> printfn "After rearrangment with CrateMover 9001, the top of the stack is %s"
