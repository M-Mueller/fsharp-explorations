module RomanNumerals

let rec internal parseLiterals (roman : list<char>) (arabic : list<int>) =
    let convertSingle c =
        match c with
        | 'I' -> Ok 1
        | 'V' -> Ok 5
        | 'X' -> Ok 10
        | 'L' -> Ok 50
        | 'C' -> Ok 100
        | 'D' -> Ok 500
        | 'M' -> Ok 1000
        | _ -> Error $"'{c}' is not a valid roman number"

    let convertTuple (a, b) =
        match (a, b) with
        | ('I', 'V') -> Ok 4
        | ('I', 'X') -> Ok 9
        | ('X', 'L') -> Ok 40
        | ('X', 'C') -> Ok 90
        | ('C', 'M') -> Ok 900
        | _ -> Error $"'{a}{b}' is not a valid roman number"

    match roman with
    | [] -> Ok arabic
    | c1 :: c2 :: cs ->
        match convertTuple (c1, c2) with
        | Ok x -> parseLiterals cs (x :: arabic)
        | Error _ ->
            convertSingle c1
            |> Result.map (fun x -> x :: arabic)
            |> Result.bind (parseLiterals (c2 :: cs))
    | c :: cs ->
        convertSingle c
        |> Result.map (fun x -> x :: arabic)
        |> Result.bind (parseLiterals cs)

let validate arabic =
    let rec validateOrder remaining =
        match remaining with
        | [] -> Ok arabic
        | d1 :: d2 :: _ when d1 > d2 -> Error "Smaller numbers cannot come before larger numbers"
        | _ :: ds -> validateOrder ds

    arabic |> validateOrder

let toArabic (roman : string) =
    parseLiterals (Seq.toList roman) []
    |> Result.bind validate
    |> Result.map (List.sum)

let literals =
    [
        ("I", 1)
        ("IV", 4)
        ("V", 5)
        ("IX", 9)
        ("X", 10)
        ("XL", 40)
        ("L", 50)
        ("XC", 90)
        ("C", 100)
        ("D", 500)
        ("CM", 900)
        ("M", 1000)
    ]

let fromArabic arabic =
    let rec convert roman arabic literals =
        match literals with
        | [] -> roman
        | (r, a) :: xs ->
            let newroman = roman + String.replicate (arabic / a) r

            convert newroman (arabic % a) xs

    if arabic <= 0 then
        Error "Roman numbers must be larger than 0"
    else
        literals
        |> List.rev
        |> convert "" arabic
        |> Ok
