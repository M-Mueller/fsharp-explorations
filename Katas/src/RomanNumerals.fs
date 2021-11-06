module RomanNumerals

let internal literals =
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

let internal literalsMap = Map literals

/// Converts each roman numeral into a number and appends the number to the list of
/// initial arabic numbers.
let rec internal parseLiterals (arabic : list<int>) (roman : list<char>) =
    let (|SingleLiteral|_|) (chars : list<char>) =
        match chars with
        | c :: cs ->
            literalsMap.TryFind (string c)
            |> Option.map (fun n -> (n, cs))
        | _ -> None

    let (|DoubleLiteral|_|) (chars : list<char>) =
        match chars with
        | c1 :: c2 :: cs ->
            literalsMap.TryFind (string c1 + string c2)
            |> Option.map (fun n -> (n, cs))
        | _ -> None

    match roman with
    | [] -> Ok arabic
    | DoubleLiteral (number, remaining) -> parseLiterals (number :: arabic) remaining
    | SingleLiteral (number, remaining) -> parseLiterals (number :: arabic) remaining
    | c :: cs -> Error $"'{c}' is not a valid roman number"

let validate arabic =
    let rec validateOrder remaining =
        match remaining with
        | [] -> Ok arabic
        | d1 :: d2 :: _ when d1 > d2 -> Error "Smaller numbers cannot come before larger numbers"
        | _ :: ds -> validateOrder ds

    arabic |> validateOrder

let toArabic (roman : string) =
    roman
    |> Seq.toList
    |> parseLiterals []
    |> Result.bind validate
    |> Result.map List.sum

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
        literals |> List.rev |> convert "" arabic |> Ok
