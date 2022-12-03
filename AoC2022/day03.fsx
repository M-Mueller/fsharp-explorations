#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

let parseInput (lines: string seq) : (char[]) list =
    lines |> Seq.map (Seq.toArray) |> Seq.toList

let itemPriority (item: char) : int =
    let p = int item

    if p >= (int 'a') then
        p - (int 'a') + 1
    else
        p - (int 'A') + 27

let rucksackPriority (items: char[]) : int =
    if items.Length % 2 = 0 then
        let mid = items.Length / 2

        let left = items[0 .. mid - 1]
        let right = items[mid..]

        let inBoth = Set.intersect (Set(left)) (Set(right))

        match Set.toList inBoth with
        | [ item ] -> itemPriority item
        | _ -> failwithf "Invalid input: more than one or zero duplicate item type"

    else
        failwithf "Invalid input: uneven number of items"

let totalRucksackPriority rucksacks =
    rucksacks |> List.map rucksackPriority |> List.sum

let badgePriority (rucksacks: char[] list) : int =
    match rucksacks with
    | [ a; b; c ] ->
        let inAll = Set.intersectMany [ Set(a); Set(b); Set(c) ]

        match Set.toList inAll with
        | [ badge ] -> itemPriority badge
        | _ -> failwithf "Invalid input: more than one or zero duplicate item type"
    | _ -> failwithf "Invalid input: group must contain exactly 3 items"

let totalBadgePriority rucksacks =
    rucksacks |> List.chunkBySize 3 |> List.map badgePriority |> List.sum



//////////////////////////
// Tests
//////////////////////////

let runTests =
    let inventory = parseInput (IO.File.ReadLines("day03.test.in"))

    test <@ inventory.Length = 6 @>
    test <@ String.Join("", inventory[2]) = "PmmdzqPrVvPwwTWBwg" @>

    test <@ itemPriority 'p' = 16 @>
    test <@ itemPriority 'L' = 38 @>

    test <@ rucksackPriority (Seq.toArray "vJrwpWtwJgWrhcsFMMfFFhFp") = 16 @>

    test <@ totalRucksackPriority inventory = 157 @>
    test <@ totalBadgePriority inventory = 70 @>


//////////////////////////
// AoC
//////////////////////////
IO.File.ReadLines("day03.in")
|> parseInput
|> totalRucksackPriority
|> printfn "Total sum of priorities is %d"

IO.File.ReadLines("day03.in")
|> parseInput
|> totalBadgePriority
|> printfn "Total sum of priorities is %d"
