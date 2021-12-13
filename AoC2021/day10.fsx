#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System


type ValidationResult =
    | Valid
    | Corrupt of char
    | Incomplete of string
    | Invalid of char


let (|OpeningChar|ClosingChar|InvalidChar|) (c: char) =
    match c with
    | '(' -> OpeningChar ')'
    | '[' -> OpeningChar ']'
    | '{' -> OpeningChar '}'
    | '<' -> OpeningChar '>'
    | ')' -> ClosingChar '('
    | ']' -> ClosingChar '['
    | '}' -> ClosingChar '{'
    | '>' -> ClosingChar '<'
    | _ -> InvalidChar


let validateLine (line: string) : ValidationResult =
    let validateChar (result: Result<char list, ValidationResult>) (character: char) =
        match result with
        | Error err -> Error err
        | Ok stack ->
            match character with
            | OpeningChar _ -> Ok(character :: stack)
            | ClosingChar matchingOpening ->
                match List.tryHead stack with
                | Some openingChar ->
                    if openingChar = matchingOpening then
                        Ok(List.tail stack)
                    else
                        Error(Corrupt character)
                | None -> Error(Corrupt character)
            | InvalidChar -> Error(Invalid character)

    let remainingUnclosed = Seq.fold validateChar (Ok []) line

    match remainingUnclosed with
    | Ok remaining ->
        if List.isEmpty remaining then
            Valid
        else
            let completion =
                remaining
                |> List.map
                    (fun opening ->
                        match opening with
                        | OpeningChar closing -> closing
                        | _ -> failwith "Stack must only include opening chars")
                |> List.toArray
                |> String.Concat

            Incomplete completion
    | Error error -> error


let syntaxErrorScore validationResult =
    match validationResult with
    | Valid -> 0
    | Corrupt c ->
        match c with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> failwithf "Not a valid Corrupt character %c" c
    | Incomplete _ -> 0
    | Invalid _ -> 0


let completionScore validationResult =
    match validationResult with
    | Valid -> 0UL
    | Corrupt _ -> 0UL
    | Incomplete completion ->
        Seq.fold
            (fun score character ->
                match character with
                | ')' -> score * 5UL + 1UL
                | ']' -> score * 5UL + 2UL
                | '}' -> score * 5UL + 3UL
                | '>' -> score * 5UL + 4UL
                | _ -> failwithf "Invalid completion character %c" character)
            0UL
            completion
    | Invalid _ -> 0UL


let totalCompletionScore validationResults =
    let sorted =
        validationResults
        |> Seq.map completionScore
        |> Seq.filter (fun score -> score > 0UL)
        |> Seq.sort
        |> Seq.toList

    List.item ((List.length sorted) / 2) sorted


/////////////////////////
/// Tests
//////////////////////////

let runTest =
    let validLines =
        [ "([])"; "{()()()}"; "<([{}])>"; "[<>({}){}[([])<>]]"; "(((((((((())))))))))" ]

    for line in validLines do
        test <@ validateLine line = Valid @>

    let invalidLines =
        [ "(]", ']'; "{()()()>", '>'; "(((()))}", '}'; "<([]){()}[{}])", ')' ]

    for line, invalid in invalidLines do
        test <@ validateLine line = Corrupt invalid @>

    let samples =
        [ "[({(<(())[]>[[{[]{<()<>>", Incomplete "}}]])})]"
          "[(()[<>])]({[<{<<[]>>(", Incomplete ")}>]})"
          "{([(<{}[<>[]}>{[]{[(<()>", Corrupt '}'
          "(((({<>}<{<{<>}{[]{[]{}", Incomplete "}}>}>))))"
          "[[<[([]))<([[{}[[()]]]", Corrupt ')'
          "[{[{({}]{}}([{[{{{}}([]", Corrupt ']'
          "{<[[]]>}<{[{[{[]{()[[[]", Incomplete "]]}}]}]}>"
          "[<(<(<(<{}))><([]([]()", Corrupt ')'
          "<{([([[(<>()){}]>(<<{{", Corrupt '>'
          "<{([{{}}[<[[[<>{}]]]>[]]", Incomplete "])}>" ]

    for line, result in samples do
        test <@ validateLine line = result @>

    let samplesSyntaxErrorScore =
        samples
        |> List.map fst
        |> List.map validateLine
        |> List.map syntaxErrorScore
        |> List.sum

    test <@ samplesSyntaxErrorScore = 26397 @>

    let completionScores =
        [ "}}]])})]", 288957UL; ")}>]})", 5566UL; "}}>}>))))", 1480781UL; "]]}}]}]}>", 995444UL; "])}>", 294UL ]

    for completion, score in completionScores do
        test <@ completionScore (Incomplete completion) = score @>

    let samplesCompletionScore =
        samples
        |> List.map fst
        |> List.map validateLine
        |> totalCompletionScore

    test <@ samplesCompletionScore = 288957UL @>


//////////////////////////
/// AoC
//////////////////////////

IO.File.ReadLines("day10.in")
|> Seq.map validateLine
|> Seq.map syntaxErrorScore
|> Seq.sum
|> printfn "The total syntax score is %d"

IO.File.ReadLines("day10.in")
|> Seq.map validateLine
|> totalCompletionScore
|> printfn "The total completion score is %d"
