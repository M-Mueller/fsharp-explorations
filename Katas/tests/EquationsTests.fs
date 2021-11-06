module EquationsTest

open Xunit
open Swensen.Unquote
open Equations.ExpressionTree

[<Fact>]
let ``evaluate ExpressionTree`` () =
    [
        (Number 3, 3)
        (Addition (Number 2, Number 5), 7)
        (Multiplication (Number 2, Number 5), 10)
        (Multiplication (Number 3, Addition (Number 2, Number 4)), 18)
        (Addition (Number 3, Multiplication (Number 2, Number 4)), 11)
        (Multiplication (Addition (Number 1, Number 2), Addition (Number 3, Number 4)), 21)
    ]
    |> List.map (fun (expr, result) -> test <@ evaluate expr = result @>)
    |> ignore

[<Fact>]
let ``fromString`` () =
    [
        ("5", Number 5)
        ("1245", Number 1245)
        ("5+3", Addition (Number 5, Number 3))
        ("5-3", Subtraction (Number 5, Number 3))
        ("5*3", Multiplication (Number 5, Number 3))
        ("5/3", Division (Number 5, Number 3))
        ("500+1234", Addition (Number 500, Number 1234))
        ("2+5*3", Addition (Number 2, Multiplication (Number 5, Number 3)))
        ("2*5+3", Addition (Multiplication (Number 2, Number 5), Number 3))
        ("2*5+3*4", Addition (Multiplication (Number 2, Number 5), Multiplication (Number 3, Number 4)))
        ("2+5*3+4", Addition (Addition (Number 2, Multiplication (Number 5, Number 3)), Number 4))
        ("2-5*3+4", Addition (Subtraction (Number 2, Multiplication (Number 5, Number 3)), Number 4))
        ("2*5-3*4", Subtraction (Multiplication (Number 2, Number 5), Multiplication (Number 3, Number 4)))
        ("2+5-3", Subtraction (Addition (Number 2, Number 5), Number 3))
        ("  5   ", Number 5)
        ("  5", Number 5)
        ("5  ", Number 5)
        ("   1245 ", Number 1245)
        ("12*25+133*45", Addition (Multiplication (Number 12, Number 25), Multiplication (Number 133, Number 45)))
        ("2 * 5 + 3 * 4", Addition (Multiplication (Number 2, Number 5), Multiplication (Number 3, Number 4)))
        ("12 * 25 + 133 * 45", Addition (Multiplication (Number 12, Number 25), Multiplication (Number 133, Number 45)))
    ]
    |> List.map (fun (equation, expected) -> test <@ fromString equation = expected @>)
    |> ignore
