#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System


type Node =
    | Start
    | End
    | BigCave of string
    | SmallCave of string

type Edge = Node * Node
type Map = Node list * Edge list
type Path = Node list


let parseInput (lines: string seq) : Map =
    let allUpper s =
        s |> Seq.map Char.IsUpper |> Seq.fold (&&) true

    let allLower s =
        s |> Seq.map Char.IsLower |> Seq.fold (&&) true

    let parseNode (node: string) : Node =
        match node with
        | "start" -> Start
        | "end" -> End
        | name when allUpper name -> BigCave name
        | name when allLower name -> SmallCave name
        | _ -> failwithf "Invalid node: %s" node

    let parseLine (line: string) : Edge =
        match line.Split('-') with
        | [| node1; node2 |] ->
            let node1 = parseNode node1
            let node2 = parseNode node2
            (node1, node2)

        | _ -> failwithf "Invalid format: %s" line

    let buildMap (edges: (Node * Node) seq) =
        let nodes =
            edges
            |> Seq.collect (fun (n1, n2) -> [ n1; n2 ])
            |> Seq.distinct
            |> Seq.toList

        (nodes, Seq.toList edges)

    lines |> Seq.map parseLine |> buildMap


let connectedNodes (node: Node) (map: Map) =
    map
    |> snd
    |> List.choose
        (fun (n1, n2) ->
            if n1 = node then Some n2
            elif n2 = node then Some n1
            else None)
    |> List.distinct


let isSmallCave node =
    match node with
    | SmallCave _ -> true
    | _ -> false


let isDeadend path =
    path
    |> List.tryLast
    |> Option.map ((<>) End)
    |> Option.defaultValue false


let filterDeadends = List.filter (isDeadend >> not)


let rec expandPath (map: Map) (specialCave: Node option) (path: Path) : Path list =
    let current = (List.head path)

    map
    |> connectedNodes current
    |> List.map
        (fun nextNode ->
            match nextNode with
            | End -> [ End :: path ]
            | Start -> []
            | SmallCave _ ->
                let numVisits =
                    path |> List.filter ((=) nextNode) |> List.length

                let canBeVisited =
                    if Some nextNode = specialCave then
                        numVisits < 2
                    else
                        numVisits < 1

                if canBeVisited then
                    expandPath map specialCave (nextNode :: path)
                else
                    [ path ]
            | BigCave _ -> expandPath map specialCave (nextNode :: path))
    |> List.concat


let listAllPaths (map: Map) =
    expandPath map None [ Start ]
    |> List.map List.rev
    |> List.distinct


let listAllPathsWithSpecialCave (map: Map) =
    let smallCaves = map |> fst |> List.filter isSmallCave

    smallCaves
    |> List.map (fun specialCave -> expandPath map (Some specialCave) [ Start ])
    |> List.concat
    |> List.map List.rev
    |> List.distinct


//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let sample1 =
        [ "start-A"; "start-b"; "A-c"; "A-b"; "b-d"; "A-end"; "b-end" ]
        |> parseInput

    let expectedNodes =
        [ Start; BigCave "A"; SmallCave "b"; SmallCave "c"; SmallCave "d"; End ]

    let expectedEdges =
        [ (Start, BigCave "A")
          (Start, SmallCave "b")
          (BigCave "A", SmallCave "c")
          (BigCave "A", SmallCave "b")
          (SmallCave "b", SmallCave "d")
          (BigCave "A", End)
          (SmallCave "b", End) ]

    test <@ fst sample1 = expectedNodes @>
    test <@ snd sample1 = expectedEdges @>

    test <@ connectedNodes Start sample1 = [ BigCave "A"; SmallCave "b" ] @>
    test <@ connectedNodes End sample1 = [ BigCave "A"; SmallCave "b" ] @>
    test <@ connectedNodes (BigCave "A") sample1 = [ Start; SmallCave "c"; SmallCave "b"; End ] @>

    test <@ isDeadend [ Start ] = true @>
    test <@ isDeadend [ Start; End ] = false @>

    test
        <@ isDeadend [ Start
                       BigCave "X"
                       SmallCave "y"
                       End ] = false @>

    test
        <@ isDeadend [ Start
                       BigCave "X"
                       SmallCave "y"
                       SmallCave "x" ] = true @>

    let sample1Paths =
        sample1 |> listAllPaths |> filterDeadends

    test <@ List.length sample1Paths = 10 @>

    let sample1SpecialPaths =
        sample1
        |> listAllPathsWithSpecialCave
        |> filterDeadends

    test <@ List.length sample1SpecialPaths = 36 @>

    let sample2 =
        [ "dc-end"; "HN-start"; "start-kj"; "dc-start"; "dc-HN"; "LN-dc"; "HN-end"; "kj-sa"; "kj-HN"; "kj-dc" ]
        |> parseInput

    test <@ sample2 |> fst |> List.length = 7 @>
    test <@ sample2 |> snd |> List.length = 10 @>

    let sample2Paths =
        sample2 |> listAllPaths |> filterDeadends

    test <@ List.length sample2Paths = 19 @>

    let sample2SpecialPaths =
        sample2
        |> listAllPathsWithSpecialCave
        |> filterDeadends

    test <@ List.length sample2SpecialPaths = 103 @>

    let sample3 =
        [ "fs-end"
          "he-DX"
          "fs-he"
          "start-DX"
          "pj-DX"
          "end-zg"
          "zg-sl"
          "zg-pj"
          "pj-he"
          "RW-he"
          "fs-DX"
          "pj-RW"
          "zg-RW"
          "start-pj"
          "he-WI"
          "zg-he"
          "pj-fs"
          "start-RW" ]
        |> parseInput

    let sample3Paths =
        sample3 |> listAllPaths |> filterDeadends

    test <@ List.length sample3Paths = 226 @>

    let sample3SpecialPaths =
        sample3
        |> listAllPathsWithSpecialCave
        |> filterDeadends

    test <@ List.length sample3SpecialPaths = 3509 @>


//////////////////////////
/// AoC
//////////////////////////

IO.File.ReadLines("day12.in")
|> parseInput
|> listAllPaths
|> filterDeadends
|> List.length
|> printfn "There are %d paths"

IO.File.ReadLines("day12.in")
|> parseInput
|> listAllPathsWithSpecialCave
|> filterDeadends
|> List.length
|> printfn "There are %d paths with a special cave"
