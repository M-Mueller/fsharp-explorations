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

let rec internal parseLiterals (roman : list<char>) (arabic : list<int>) =
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
    | DoubleLiteral (a, cs) -> parseLiterals cs (a :: arabic)
    | SingleLiteral (a, cs) -> parseLiterals cs (a :: arabic)
    | c :: cs -> Error $"'{c}' is not a valid roman number"

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
