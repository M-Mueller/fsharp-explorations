module EquationsTest

open Xunit
open FsUnit.Xunit
open Equations.ExpressionTree

type ClassDataBase(generator : obj [] seq) = 
    interface seq<obj []> with
        member this.GetEnumerator() = generator.GetEnumerator()
        member this.GetEnumerator() = 
            generator.GetEnumerator() :> System.Collections.IEnumerator

type ``Evaluate a ExpressionTree of `` () =
    [<Fact>]
    member x.``3 is 3`` () =
        evaluate (Number 3) |> should equal 3

    [<Fact>]
    member x.``2 + 5 is 7`` () =
        evaluate (Addition (Number 2, Number 5)) |> should equal 7

    [<Fact>]
    member x.``2 * 5 is 10`` () =
        evaluate (Multiplication (Number 2, Number 5)) |> should equal 10

    [<Fact>]
    member x.``3 * (2 + 4) is 18`` () =
        evaluate (Multiplication (Number 3, Addition (Number 2, Number 4))) |> should equal 18

    [<Fact>]
    member x.``3 + (2 * 4) is 11`` () =
        evaluate (Addition (Number 3, Multiplication (Number 2, Number 4))) |> should equal 11

    [<Fact>]
    member x.``(1 + 2) * (3 + 4) is 21`` () =
        evaluate (Multiplication (Addition (Number 1, Number 2), Addition (Number 3, Number 4))) |> should equal 21

type Equations () = 
    inherit ClassDataBase([
        [| "5"; Number 5 |];
        [| "1245"; Number 1245 |];
        [| "5+3"; Addition (Number 5, Number 3) |];
        [| "5-3"; Subtraction (Number 5, Number 3) |];
        [| "5*3"; Multiplication (Number 5, Number 3) |];
        [| "5/3"; Division (Number 5, Number 3) |];
        [| "500+1234"; Addition (Number 500, Number 1234) |];
        [| "2+5*3"; Addition (Number 2, Multiplication (Number 5, Number 3)) |];
        [| "2*5+3"; Addition (Multiplication (Number 2, Number 5), Number 3) |];
        [| "2*5+3*4"; Addition (Multiplication (Number 2, Number 5), Multiplication (Number 3, Number 4)) |];
        [| "2+5*3+4"; Addition (Addition (Number 2, Multiplication (Number 5, Number 3)), Number 4) |];
        [| "2-5*3+4"; Addition (Subtraction (Number 2, Multiplication (Number 5, Number 3)), Number 4) |];
        [| "2*5-3*4"; Subtraction (Multiplication (Number 2, Number 5), Multiplication (Number 3, Number 4)) |];
        [| "2+5-3"; Subtraction (Addition (Number 2, Number 5), Number 3) |];
        [| "  5   "; Number 5 |];
        [| "  5"; Number 5 |];
        [| "5  "; Number 5 |];
        [| "   1245 "; Number 1245 |];
        [| "12*25+133*45"; Addition (Multiplication (Number 12, Number 25), Multiplication (Number 133, Number 45)) |];
        [| "2 * 5 + 3 * 4"; Addition (Multiplication (Number 2, Number 5), Multiplication (Number 3, Number 4)) |];
        [| "12 * 25 + 133 * 45"; Addition (Multiplication (Number 12, Number 25), Multiplication (Number 133, Number 45)) |];
    ])

[<Theory>]
[<ClassData(typeof<Equations>)>]
let ```fromString`` (equation : string, tree : ExpressionTree) =
    fromString equation |> should equal tree
