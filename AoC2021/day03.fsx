#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote

let charToBit (c: char) =
    match c with
    | '1' -> 1
    | '0' -> 0
    | _ -> failwithf "Invalid bit: %c" c

let binaryArrayToDecimal (binary: char array) =
    binary
    |> Array.map charToBit
    |> Array.map uint32
    |> Array.fold (fun deci bit -> (deci <<< 1) + bit) 0u
    |> int

let binaryToDecimal (binary: string) =
    binaryArrayToDecimal (binary.ToCharArray())

let mostCommonBitAtIndex (index: int) (lines: string seq) : char =
    lines
    |> Seq.fold (fun count1 line -> count1 + (charToBit line.[index])) 0
    |> fun count ->
        if (float count) >= (float (Seq.length lines) / 2.0) then
            '1'
        else
            '0'

let leastCommonBitAtIndex index lines =
    if mostCommonBitAtIndex index lines = '0' then
        '1'
    else
        '0'

let powerConsumption (lines: string seq) =
    let numDigits = lines |> Seq.head |> String.length

    let gamma =
        { 0 .. numDigits - 1 }
        |> Seq.map (fun index -> mostCommonBitAtIndex index lines)
        |> Seq.toArray
        |> binaryArrayToDecimal

    let epsilon =
        int (~~~(uint gamma) &&& ((1u <<< numDigits) - 1u))

    gamma * epsilon

let filterByBitAtIndex (index: int) (bit: char) =
    Seq.filter (fun (line: string) -> line.[index] = bit)

let rec lifeSupportRating (selectBitAtIndex: int -> string seq -> char) (index: int) (lines: string seq) : string =
    let numLines = Seq.length lines

    if numLines = 1 then
        Seq.head lines
    else
        lines
        |> filterByBitAtIndex index (selectBitAtIndex index lines)
        |> lifeSupportRating selectBitAtIndex (index + 1)

let oxygenGeneratorRating =
    lifeSupportRating mostCommonBitAtIndex 0
    >> binaryToDecimal

let co2ScrupperRating =
    lifeSupportRating leastCommonBitAtIndex 0
    >> binaryToDecimal

//////////////////////////
/// Tests
//////////////////////////

let runTests =
    test <@ binaryToDecimal "10111" = 23 @>
    test <@ binaryToDecimal "01010" = 10 @>

    let report =
        [ "00100"
          "11110"
          "10110"
          "10111"
          "10101"
          "01111"
          "00111"
          "11100"
          "10000"
          "11001"
          "00010"
          "01010" ]
        |> List.toSeq

    let reportFiltered =
        [ "11110"
          "10110"
          "10111"
          "10101"
          "11100"
          "10000"
          "11001" ]
        |> List.toSeq

    test <@ mostCommonBitAtIndex 0 report = '1' @>
    test <@ mostCommonBitAtIndex 1 report = '0' @>
    test <@ mostCommonBitAtIndex 1 reportFiltered = '0' @>
    test <@ leastCommonBitAtIndex 0 report = '0' @>
    test <@ leastCommonBitAtIndex 1 report = '1' @>
    test <@ leastCommonBitAtIndex 1 reportFiltered = '1' @>

    test <@ (filterByBitAtIndex 0 '1' report |> Seq.toList) = (reportFiltered |> Seq.toList) @>

    test <@ powerConsumption report = 198 @>
    test <@ oxygenGeneratorRating report = 23 @>
    test <@ co2ScrupperRating report = 10 @>

    ()

//////////////////////////
/// AoC Results
//////////////////////////

let report = System.IO.File.ReadLines("day03.in")

printfn "Current power consumption is %d" (powerConsumption report)

printfn
    "Current life support rating is %d"
    (oxygenGeneratorRating report
     * co2ScrupperRating report)
