module RomanNumeralsTests

open Xunit
open FsUnit.Xunit
open RomanNumerals
open TestUtils

type RomanNumbers () = 
    inherit ClassDataBase([
        [| "I"; 1 |]
        [| "III"; 3 |]
        [| "IV"; 4 |]
        [| "V"; 5 |]
        [| "DXV"; 515 |]
        [| "DCCLXXXIX"; 789 |]
        [| "MDCCCLXXXVIII"; 1888 |]
        [| "MCMLXXXIV"; 1984 |]
        [| "MCMXCIX"; 1999 |]
        [| "MMI"; 2001 |]
        [| "MMVI"; 2006 |]
        [| ""; 0 |]
    ])

[<Theory>]
[<ClassData(typeof<RomanNumbers>)>]
let ```valid roman numbers`` (roman : string, arabic : int) =
    let (result:Result<int, string>) = Ok arabic
    convertToArabic roman |> should equal result

type InvalidRomanNumbers () = 
    inherit ClassDataBase([
        [| "IIV"; "Smaller numbers cannot come before larger numbers" |]
        [| "XM"; "Smaller numbers cannot come before larger numbers" |]
        [| "DCCLIIXXXIX"; "Smaller numbers cannot come before larger numbers" |]
        [| "ABC"; "'A' is not a valid roman number" |]
        [| "V II"; "' ' is not a valid roman number" |]
    ])

[<Theory>]
[<ClassData(typeof<InvalidRomanNumbers>)>]
let ```invalid roman numbers`` (roman : string) (error : string) =
    let (result:Result<int, string>) = Error error
    convertToArabic roman |> should equal result
