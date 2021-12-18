#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System


let parseInput (lines: string seq) =
    let polymer = Seq.head lines

    let parseInsertionRule (line: string) =
        match line.Split(" -> ") with
        | [| a; b |] -> (a, b)
        | _ -> failwithf "Invalid rule: %s" line

    let insertionRules =
        lines
        |> Seq.tail
        |> Seq.tail
        |> Seq.map parseInsertionRule
        |> (fun rules ->
            assert (rules = Seq.distinct rules)
            rules)
        |> Map.ofSeq

    (polymer, insertionRules)


let incrementCount key inc =
    Map.change
        key
        (Option.map ((+) inc)
         >> Option.defaultValue inc
         >> Some)


let countElementPairs (polymer: string) : Map<string, uint64> =
    polymer
    |> Seq.pairwise
    |> Seq.map String.Concat
    |> Seq.fold
        (fun pairCounts pair ->
            pairCounts
            |> Map.change
                pair
                (fun count ->
                    match count with
                    | Some count -> Some(count + 1UL)
                    | None -> Some 1UL))
        Map.empty


let evolvePolymer (polymer: string) (rules: Map<string, string>) (steps: int) =
    let mutable elementCounts = polymer |> countElementPairs

    for step in 1 .. steps do
        elementCounts <-
            elementCounts
            |> Map.fold
                (fun newCounts pair count ->
                    match rules.TryFind pair with
                    | Some element ->
                        newCounts
                        |> incrementCount ((string pair.[0]) + element) count
                        |> incrementCount (element + (string pair.[1])) count
                    | None -> newCounts |> incrementCount pair count)
                Map.empty

    elementCounts


let polymerRating (elementCounts: Map<string, uint64>) =
    let singleElementCounts =
        elementCounts
        |> Map.toList
        |> List.map (fun (pair, count) -> [ (pair.[0], count); (pair.[1], count) ])
        |> List.concat
        |> List.groupBy fst
        |> List.map (fun (element, counts) -> (element, counts |> List.map snd |> List.sum))
        |> List.map
            (fun (element, count) ->

                (element, uint64 (ceil ((float count) / 2.0))))

    let min =
        singleElementCounts |> List.map snd |> List.min

    let max =
        singleElementCounts |> List.map snd |> List.max

    max - min

//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let polymer, insertionRules =
        IO.File.ReadLines("day14.test.in") |> parseInput

    let expectedInsertionRules =
        [ "CH", "B"
          "HH", "N"
          "CB", "H"
          "NH", "C"
          "HB", "C"
          "HC", "B"
          "HN", "C"
          "NN", "C"
          "BH", "H"
          "NC", "B"
          "NB", "B"
          "BN", "B"
          "BB", "N"
          "BC", "B"
          "CC", "N"
          "CN", "C" ]
        |> Map.ofList

    test <@ polymer = "NNCB" @>
    test <@ insertionRules = expectedInsertionRules @>

    let expectedAfterStep1 = "NCNBCHB" |> countElementPairs
    let expectedAfterStep2 = "NBCCNBBBCBHCB" |> countElementPairs

    let expectedAfterStep3 =
        "NBBBCNCCNBBNBNBBCHBHHBCHB" |> countElementPairs

    let expectedAfterStep4 =
        "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
        |> countElementPairs

    test <@ evolvePolymer polymer insertionRules 1 = expectedAfterStep1 @>
    test <@ evolvePolymer polymer insertionRules 2 = expectedAfterStep2 @>
    test <@ evolvePolymer polymer insertionRules 3 = expectedAfterStep3 @>
    test <@ evolvePolymer polymer insertionRules 4 = expectedAfterStep4 @>

    test
        <@ evolvePolymer polymer insertionRules 10
           |> polymerRating = 1588UL @>
    test
        <@ evolvePolymer polymer insertionRules 40
           |> polymerRating = 2188189693529UL @>

//////////////////////////
/// AoC
//////////////////////////

IO.File.ReadLines("day14.in")
|> parseInput
|> (fun (polymer, rules) -> evolvePolymer polymer rules 10)
|> polymerRating
|> printfn "The polymer rating after 10 steps is %d"


IO.File.ReadLines("day14.in")
|> parseInput
|> (fun (polymer, rules) -> evolvePolymer polymer rules 40)
|> polymerRating
|> printfn "The polymer rating after 40 steps is %d"
