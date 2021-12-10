#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System


let parseInput (lines: string seq) =
    let splitDigits (line: string) =
        line.Split(" ", StringSplitOptions.RemoveEmptyEntries)

    let parseLine (line: string) =
        match line.Split("|") with
        | [| uniqueSignals; outputValues |] -> (splitDigits uniqueSignals, splitDigits outputValues)
        | _ -> failwithf "Invalid input: %s" line

    Seq.map parseLine lines


let countSimpleDigits (digitSegments: string seq) =
    let convertDigit digit =
        match Seq.length digit with
        | 2 -> Some 1
        | 3 -> Some 7
        | 4 -> Some 4
        | 7 -> Some 8
        | 5 -> None
        | 6 -> None
        | _ -> failwithf "Invalid digit: %s" digit

    digitSegments
    |> Seq.choose convertDigit
    |> Seq.length


type PatternMap =
    { ``1``: Set<char>
      ``4``: Set<char>
      ``7``: Set<char>
      ``8``: Set<char>
      ``235``: Set<char> list
      ``069``: Set<char> list }


let groupPatterns (digitSegments: string seq) : PatternMap =
    let convertDigit (map: PatternMap) digit =
        match Set.count digit with
        | 2 -> { map with ``1`` = digit }
        | 3 -> { map with ``7`` = digit }
        | 4 -> { map with ``4`` = digit }
        | 7 -> { map with ``8`` = digit }
        | 5 ->
            { map with
                  ``235`` = digit :: map.``235`` }
        | 6 ->
            { map with
                  ``069`` = digit :: map.``069`` }
        | _ -> failwithf "Invalid digit: %A" digit

    let map =
        { ``1`` = Set.empty
          ``4`` = Set.empty
          ``7`` = Set.empty
          ``8`` = Set.empty
          ``235`` = []
          ``069`` = [] }

    digitSegments
    |> Seq.map (fun str -> Set.ofArray (str.ToCharArray()))
    |> Seq.fold convertDigit map


let mapWiresToSegments (digits: string seq) : (Map<char, char>) =
    let patternMap = groupPatterns digits

    let singleItem (s: Set<char>) =
        match Set.toList s with
        | [ v ] -> v
        | _ -> failwithf "Set should only have a single item: %A" s

    // 7 and 1 have 2 segments in common, so the third segment of 7 must map to 'a'
    let segA =
        (Set.difference patternMap.``7`` patternMap.``1``
         |> singleItem)

    // 4, 2, 3 and 5 only have 'd' in common
    let segD =
        (Set.intersectMany (patternMap.``4`` :: patternMap.``235``)
         |> singleItem)

    // With 'd' known we can derive 'b' from 4 and 1
    let segB =
        (Set.difference patternMap.``4`` patternMap.``1``
         |> Set.remove segD
         |> singleItem)

    // 2, 3 and 5 have 'a', 'd' and 'g' in common and we know 'a' and 'd'
    let segG =
        Set.intersectMany patternMap.``235``
        |> Set.remove segA
        |> Set.remove segD
        |> singleItem

    // 5 is the only number in this set with 'b' and only one unknown by now
    let segF =
        patternMap.``235``
        |> List.filter (fun digit -> Set.contains segB digit)
        |> List.head
        |> Set.remove segA
        |> Set.remove segD
        |> Set.remove segG
        |> Set.remove segB
        |> singleItem

    let segC =
        patternMap.``1`` |> Set.remove segF |> singleItem

    let segE =
        patternMap.``8``
        |> Set.remove segA
        |> Set.remove segB
        |> Set.remove segC
        |> Set.remove segD
        |> Set.remove segF
        |> Set.remove segG
        |> singleItem

    Map [ (segA, 'a')
          (segB, 'b')
          (segC, 'c')
          (segD, 'd')
          (segE, 'e')
          (segF, 'f')
          (segG, 'g') ]


let decodeDigit (segmentMap: Map<char, char>) (digits: string) : int =
    let digitToDecimal (segments: string) =
        match segments with
        | "abcefg" -> 0
        | "cf" -> 1
        | "acdeg" -> 2
        | "acdfg" -> 3
        | "bcdf" -> 4
        | "abdfg" -> 5
        | "abdefg" -> 6
        | "acf" -> 7
        | "abcdefg" -> 8
        | "abcdfg" -> 9
        | _ -> failwithf "Invalid segments: %s" segments

    digits.ToCharArray()
    |> Array.map (fun c -> segmentMap.[c])
    |> Array.sort
    |> String.Concat
    |> digitToDecimal


let decodeOutput (uniqueDigits: string seq) (output: string seq) : int =
    let segmentMap = mapWiresToSegments uniqueDigits

    output
    |> Seq.map (decodeDigit segmentMap)
    |> Seq.fold (fun combined digit -> combined * 10 + digit) 0

//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let testInput =
        IO.File.ReadLines("day08.test.in")
        |> parseInput
        |> Seq.toList

    test
        <@ fst testInput.[0] = [| "be"
                                  "cfbegad"
                                  "cbdgef"
                                  "fgaecd"
                                  "cgeb"
                                  "fdcge"
                                  "agebfd"
                                  "fecdb"
                                  "fabcd"
                                  "edb" |] @>

    test <@ snd testInput.[0] = [| "fdgacbe"; "cefdb"; "cefbgd"; "gcbe" |] @>

    let numSimpleDigits =
        testInput
        |> List.map snd
        |> List.map countSimpleDigits
        |> List.sum

    test <@ numSimpleDigits = 26 @>

    let sample =
        [ "acedgfb"; "cdfbe"; "gcdfa"; "fbcad"; "dab"; "cefabd"; "cdfgeb"; "eafb"; "cagedb"; "ab" ]

    let expectedMap =
        Map [ ('d', 'a')
              ('e', 'b')
              ('a', 'c')
              ('f', 'd')
              ('g', 'e')
              ('b', 'f')
              ('c', 'g') ]

    test <@ mapWiresToSegments sample = expectedMap @>

    test <@ decodeDigit expectedMap "cdfeb" = 5 @>
    test <@ decodeDigit expectedMap "fcadb" = 3 @>
    test <@ decodeDigit expectedMap "cdfeb" = 5 @>
    test <@ decodeDigit expectedMap "cdbaf" = 3 @>

    let expectedOutput =
        [ 8394; 9781; 1197; 9361; 4873; 8418; 4548; 1625; 8717; 4315 ]

    for (uniqueDigits, output), expected in List.zip testInput expectedOutput do
        test <@ decodeOutput uniqueDigits output = expected @>

//////////////////////////
/// AoC
//////////////////////////
let input =
    IO.File.ReadLines("day08.in") |> parseInput

input
|> Seq.map snd
|> Seq.map countSimpleDigits
|> Seq.sum
|> printfn "There are %d instances of 1, 4, 7 or 8"

input
|> Seq.map (fun (unique, output) -> decodeOutput unique output)
|> Seq.sum
|> printfn "The sum of all output values is %d"
