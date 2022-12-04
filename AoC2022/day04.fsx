#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System
open System.Text.RegularExpressions


type SectionPair = (int * int) * (int * int)


let parseInput (lines: string seq) : SectionPair[] =
    let regex = Regex(@"(\d+)-(\d+),(\d+)-(\d+)", RegexOptions.Compiled)

    let parseIntoPair (line: string) : SectionPair =
        let m = regex.Match(line)

        if m.Success then
            assert (m.Groups.Count = 5)
            let values = m.Groups |> Seq.tail |> Seq.toArray |> Array.map (fun g -> int g.Value)

            (values[0], values[1]), (values[2], values[3])
        else
            failwithf "Invalid input: line doesn't match pattern %s" line

    lines |> Seq.toArray |> Array.map parseIntoPair


let fullyContainsSection (start1, end1) (start2, end2) =
    (start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)


let sectionsOverlap (start1, end1) (start2, end2) =
    (start2 <= end1 && start1 <= end2)
    || (start1 <= end2 && start2 <= end1)
    || fullyContainsSection (start1, end1) (start2, end2)


let totalFullyContained assignments =
    assignments
    |> Array.filter (fun (a, b) -> fullyContainsSection a b)
    |> Array.length


let totalOverlapping assignments =
    assignments |> Array.filter (fun (a, b) -> sectionsOverlap a b) |> Array.length

//////////////////////////
// Tests
//////////////////////////

let runTests =
    let assignments = parseInput (IO.File.ReadLines("day04.test.in"))

    test <@ assignments.Length = 6 @>
    test <@ assignments[0] = ((2, 4), (6, 8)) @>
    test <@ assignments[3] = ((2, 8), (3, 7)) @>

    test <@ fullyContainsSection (2, 8) (3, 7) = true @>
    test <@ fullyContainsSection (2, 4) (6, 8) = false @>
    test <@ fullyContainsSection (6, 6) (4, 6) = true @>
    test <@ fullyContainsSection (6, 6) (6, 6) = true @>
    test <@ fullyContainsSection (5, 6) (6, 7) = false @>
    test <@ fullyContainsSection (1, 2) (0, 1) = false @>

    test <@ sectionsOverlap (5, 7) (7, 9) = true @>
    test <@ sectionsOverlap (2, 8) (3, 7) = true @>
    test <@ sectionsOverlap (6, 6) (4, 6) = true @>
    test <@ sectionsOverlap (2, 6) (4, 8) = true @>
    test <@ sectionsOverlap (2, 3) (4, 5) = false @>
    test <@ sectionsOverlap (4, 5) (2, 3) = false @>

    test <@ totalFullyContained assignments = 2 @>


//////////////////////////
// AoC
//////////////////////////

let assignments = parseInput (IO.File.ReadLines("day04.in"))

assignments
|> totalFullyContained
|> printfn "%d assignment pairs contain a range that is fully contained in the other"

assignments
|> totalOverlapping
|> printfn "%d assignment pairs contain overlapping sections"
