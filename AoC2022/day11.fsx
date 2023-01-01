#r "nuget: Unquote, 6.1.0"

#load "Common.fsx"

open Common
open Swensen.Unquote
open System
open System.Text.RegularExpressions

type Operation =
    | Addition of int64
    | Product of int64
    | Square

type Monkey =
    { items: int64 list
      itemsInspected: int64
      operation: Operation
      divisibleBy: int64
      onTrue: int
      onFalse: int }


let parseStartingItems (line: string) =
    match line.Split(":") with
    | [| _; items |] -> items.Split(",") |> Array.map int64 |> Array.toList
    | _ -> failwithf "Invalid starting items: %s" line


let parseOperation (operation: string) : Operation =
    let regex =
        Regex(@"\s*Operation: new = old (\*|\+) (old|\d+)", RegexOptions.Compiled)

    let m = regex.Match(operation)

    if m.Success then
        assert (m.Groups.Count = 3)

        let rhs = m.Groups[2].Value

        if rhs = "old" then
            assert (m.Groups[1].Value = "*")
            Square
        else
            let rhs = int64 rhs

            match m.Groups[1].Value with
            | "*" -> Product rhs
            | "+" -> Addition rhs
            | _ -> failwithf "Unsupported op: %s" m.Groups[1].Value

    else
        failwithf "Invalid operation: %s" operation


let parseTest (test: string) =
    let regex = Regex(@"\s*Test: divisible by (\d+)", RegexOptions.Compiled)
    let m = regex.Match(test)

    if m.Success then
        int64 m.Groups[1].Value
    else
        failwithf "Invalid test: %s" test


let parseEffect (effect: string) =
    let regex =
        Regex(@"\s*If (true|false): throw to monkey (\d+)", RegexOptions.Compiled)

    let m = regex.Match(effect)

    if m.Success then
        int m.Groups[2].Value
    else
        failwithf "Invalid effect: %s" effect


let parseInput filename =
    let parseMonkey monkey =
        if List.length monkey <> 6 then
            failwithf "Invalid input: Expected 6 lines got %A" monkey

        { items = parseStartingItems monkey[1]
          itemsInspected = 0L
          operation = parseOperation monkey[2]
          divisibleBy = parseTest monkey[3]
          onTrue = parseEffect monkey[4]
          onFalse = parseEffect monkey[5] }

    IO.File.ReadLines(filename)
    |> Seq.toList
    |> List.splitOn String.IsNullOrWhiteSpace
    |> List.map parseMonkey


type ItemToThrow = { item: int64; toMonkey: int }


let processMonkey (monkey: Monkey) : ItemToThrow list =
    monkey.items
    |> List.map (fun item ->
        let newItem =
            match monkey.operation with
            | Addition rhs -> item + rhs
            | Product rhs -> item * rhs
            | Square -> item * item

        let newItem = newItem / 3L

        assert (newItem >= 0)

        if newItem % monkey.divisibleBy = 0 then
            { item = newItem
              toMonkey = monkey.onTrue }
        else
            { item = newItem
              toMonkey = monkey.onFalse })


let simulateRound (monkeys: Monkey list) : Monkey list =
    let mutable monkeys = monkeys

    for i in 0 .. (monkeys.Length - 1) do
        let itemsToThrow = processMonkey monkeys[i]
        assert (itemsToThrow.Length = monkeys[i].items.Length)

        monkeys <-
            List.updateAt
                i
                { monkeys[i] with
                    items = []
                    itemsInspected = monkeys[i].itemsInspected + (int64 itemsToThrow.Length) }
                monkeys

        for itemToThrow in itemsToThrow do
            let m = itemToThrow.toMonkey
            assert (m <> i)
            assert (m < monkeys.Length)
            monkeys <- List.updateAt m { monkeys[m] with items = monkeys[m].items @ [ itemToThrow.item ] } monkeys

    monkeys


let simulateRounds rounds (monkeys: Monkey list) : Monkey list =
    List.fold (fun monkeys _ -> simulateRound monkeys) monkeys [ 0 .. rounds - 1 ]


let monkeyBusiness rounds (monkeys: Monkey list) : int64 =
    monkeys
    |> simulateRounds rounds
    |> List.map (fun m -> m.itemsInspected)
    |> List.sortDescending
    |> List.take 2
    |> List.fold (*) 1L


//////////////////////////
// Tests
//////////////////////////

let tests =
    test <@ parseOperation "Operation: new = old + 7" = Addition 7 @>
    test <@ parseOperation "Operation: new = old * old" = Square @>
    test <@ parseOperation "Operation: new = old * 2" = Product 2 @>

    test <@ parseTest "Test: divisible by 3" = 3 @>

    let monkeys = parseInput "day11.test.in"
    test <@ List.length monkeys = 4 @>

    test <@ monkeys[0].items = [ 79; 98 ] @>
    test <@ monkeys[0].operation = Product 19 @>
    test <@ monkeys[0].divisibleBy = 23 @>
    test <@ monkeys[0].onTrue = 2 @>
    test <@ monkeys[0].onFalse = 3 @>

    test <@ processMonkey monkeys[0] = [ { item = 500; toMonkey = 3 }; { item = 620; toMonkey = 3 } ] @>

    let itemsAfterRound1 = monkeys |> simulateRound |> List.map (fun m -> m.items)
    test <@ itemsAfterRound1 = [ [ 20; 23; 27; 26 ]; [ 2080; 25; 167; 207; 401; 1046 ]; []; [] ] @>

    let itemsAfterRound10 = monkeys |> simulateRounds 10 |> List.map (fun m -> m.items)
    test <@ itemsAfterRound10 = [ [ 91; 16; 20; 98 ]; [ 481; 245; 22; 26; 1092; 30 ]; []; [] ] @>

    let itemsInspectedAfterRound20 =
        monkeys |> simulateRounds 20 |> List.map (fun m -> m.itemsInspected)

    test <@ itemsInspectedAfterRound20 = [ 101; 95; 7; 105 ] @>

    test <@ monkeyBusiness 20 monkeys = 10605L @>

//////////////////////////
// AoC
//////////////////////////

parseInput "day11.in"
|> monkeyBusiness 20
|> printfn "Monkey business after 20 rounds is %d"
