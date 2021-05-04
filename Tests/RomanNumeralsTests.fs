module RomanNumeralsTests

open Xunit
open FsCheck.Xunit
open RomanNumerals
open Swensen.Unquote

let validRomanNumbers =
    [
        ("I", 1)
        ("III", 3)
        ("IV", 4)
        ("V", 5)
        ("DXV", 515)
        ("DCCLXXXIX", 789)
        ("MDCCCLXXXVIII", 1888)
        ("MCMLXXXIV", 1984)
        ("MCMXCIX", 1999)
        ("MMI", 2001)
        ("MMVI", 2006)
    ]

[<Fact>]
let ``convert valid roman numbers to arabic`` () =
    validRomanNumbers
    |> List.map (fun (roman, arabic) -> test <@ toArabic roman = Ok arabic @>)
    |> ignore

[<Fact>]
let ``convert valid arabic numbers to roman`` () =
    validRomanNumbers
    |> List.map (fun (roman, arabic) -> test <@ fromArabic arabic = Ok roman @>)
    |> ignore

[<Property>]
let ``convert roundtrip`` (arabic:int) =
    if arabic <= 0 then
        arabic |> fromArabic = (Error "Roman numbers must be larger than 0")
    else
        arabic |> fromArabic |> Result.bind toArabic = (Ok arabic)


let invalidRomanNumbers =
    [
        ("IIV", "Smaller numbers cannot come before larger numbers")
        ("XM", "Smaller numbers cannot come before larger numbers")
        ("DCCLIIXXXIX", "Smaller numbers cannot come before larger numbers")
        ("ABC", "'A' is not a valid roman number")
        ("V II", "' ' is not a valid roman number")
    ]

[<Fact>]
let ``test invalid roman numbers`` () =
    invalidRomanNumbers
    |> List.map (fun (roman, error) -> test <@ toArabic roman = Error error @>)
    |> ignore
