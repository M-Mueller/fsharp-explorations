#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

let readInput (filepath: string) : int[,] =
    let lines: string seq = IO.File.ReadLines(filepath)

    lines
    |> Seq.map (Seq.toArray >> Array.map (string >> int))
    |> Seq.toArray
    |> array2D


let treesToLeft y x (map: int[,]) : int[] = map[y, 0 .. (x - 1)] |> Array.rev


let treesToRight y x (map: int[,]) : int[] =
    map[y, (x + 1) .. (Array2D.length2 map)]


let treesToTop y x (map: int[,]) : int[] = map[0 .. (y - 1), x] |> Array.rev


let treesToBottom y x (map: int[,]) : int[] =
    map[(y + 1) .. (Array2D.length1 map), x]


let determineVisibility (map: int[,]) : bool[,] =
    let length1 = Array2D.length1 map
    let length2 = Array2D.length2 map

    let isMax h trees =
        if Array.isEmpty trees then true else Array.max trees < h

    let isVisible y x h =
        isMax h (treesToLeft y x map)
        || isMax h (treesToRight y x map)
        || isMax h (treesToBottom y x map)
        || isMax h (treesToTop y x map)

    Array2D.mapi isVisible map


let numVisible (visibility: bool[,]) : int =
    visibility |> Seq.cast<bool> |> Seq.sumBy (fun v -> if v then 1 else 0)


let determineScenicScores (map: int[,]) : int[,] =
    let scenicScore y x h =
        let numTreesInDir trees : int =
            trees
            |> Array.tryFindIndex (fun v -> v >= h)
            |> Option.map ((+) 1)
            |> Option.defaultValue (Array.length trees)

        let left = numTreesInDir (treesToLeft y x map)
        let right = numTreesInDir (treesToRight y x map)
        let top = numTreesInDir (treesToTop y x map)
        let bottom = numTreesInDir (treesToBottom y x map)

        left * right * top * bottom

    Array2D.mapi scenicScore map


let maxScenicScore (scores: int[,]) : int = scores |> Seq.cast<int> |> Seq.max

//////////////////////////
// Tests
//////////////////////////

let tests =
    let map = readInput ("day08.test.in")

    test
        <@
            map = array2D
                      [| [| 3; 0; 3; 7; 3 |]
                         [| 2; 5; 5; 1; 2 |]
                         [| 6; 5; 3; 3; 2 |]
                         [| 3; 3; 5; 4; 9 |]
                         [| 3; 5; 3; 9; 0 |] |]
        @>

    test <@ treesToLeft 1 2 map = [| 5; 2 |] @>
    test <@ treesToRight 1 2 map = [| 1; 2 |] @>
    test <@ treesToTop 1 2 map = [| 3 |] @>
    test <@ treesToBottom 1 2 map = [| 3; 5; 3 |] @>

    let visibility = determineVisibility map

    test
        <@
            visibility = array2D
                             [| [| true; true; true; true; true |]
                                [| true; true; true; false; true |]
                                [| true; true; false; true; true |]
                                [| true; false; true; false; true |]
                                [| true; true; true; true; true |] |]
        @>

    test <@ numVisible visibility = 21 @>

    let scenicScores = determineScenicScores map

    test <@ scenicScores[1, 2] = 4 @>
    test <@ scenicScores[3, 2] = 8 @>

    test <@ maxScenicScore scenicScores = 8 @>


//////////////////////////
// AoC
//////////////////////////

readInput "day08.in"
|> determineVisibility
|> numVisible
|> printfn "There are %d trees visible"

readInput "day08.in"
|> determineScenicScores
|> maxScenicScore
|> printfn "The highest scenic score is %d"
