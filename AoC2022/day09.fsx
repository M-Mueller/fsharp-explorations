#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

type Motion =
    | Right
    | Left
    | Up
    | Down


type Point =
    { x: int
      y: int }

    static member (+)(a, b) = { x = a.x + b.x; y = a.y + b.y }
    static member (-)(a, b) = { x = a.x - b.x; y = a.y - b.y }


type Rope = Point list


type RopeHistory = { rope: Rope; visited: Point list }


let readInput (filepath: string) : Motion list =
    let lines: string seq = IO.File.ReadLines(filepath)

    let parseMotion motion =
        match motion with
        | "R" -> Right
        | "L" -> Left
        | "U" -> Up
        | "D" -> Down
        | _ -> failwithf "Invalid motion: %s" motion

    let parseLine (line: string) : Motion list =
        match line.Split(" ") with
        | [| motion; amount |] -> List.init (int amount) (fun _ -> parseMotion motion)
        | _ -> failwithf "Invalid input: %s" line

    lines |> Seq.map parseLine |> Seq.concat |> Seq.toList

let rec updateTail (rope: Rope) : Rope =
    match rope with
    | [] -> rope
    | head :: [] -> rope
    | head :: segment :: tail ->
        let distance = head - segment

        assert (abs distance.x <= 2)
        assert (abs distance.y <= 2)

        let newSegment =
            if abs distance.x <= 1 && abs distance.y <= 1 then
                segment
            else
                let move =
                    if distance.x <> 0 && distance.y <> 0 then
                        { x = sign distance.x
                          y = sign distance.y }
                    else
                        { x = if (abs distance.x) >= 2 then (sign distance.x) else 0
                          y = if (abs distance.y) >= 2 then (sign distance.y) else 0 }

                segment + move

        head :: (updateTail (newSegment :: tail))


let moveRope rope motion =
    let newRope =
        match rope with
        | [] -> []
        | head :: tail ->
            match motion with
            | Right -> { x = head.x + 1; y = head.y } :: tail
            | Left -> { x = head.x - 1; y = head.y } :: tail
            | Up -> { x = head.x; y = head.y + 1 } :: tail
            | Down -> { x = head.x; y = head.y - 1 } :: tail

    updateTail newRope


let simulateMotions (length: int) (motions: Motion list) : RopeHistory =
    let initialRope = List.init length (fun _ -> { x = 0; y = 0 })

    let initialHistory =
        { rope = initialRope
          visited = [ List.last initialRope ] }

    let folder state motion =
        let newRope = moveRope state.rope motion

        { rope = newRope
          visited = (List.last newRope) :: state.visited }

    List.fold folder initialHistory motions


let numberOfVisitedPositions ropeHistory =
    ropeHistory.visited |> Set.ofList |> Set.count

//////////////////////////
// Tests
//////////////////////////

let tests =
    let motions = readInput ("day09.test.in")

    test <@ List.take 12 motions = [ Right; Right; Right; Right; Up; Up; Up; Up; Left; Left; Left; Down ] @>

    let overlapping = [ { x = 2; y = 3 }; { x = 2; y = 3 } ]
    test <@ updateTail overlapping = overlapping @>

    let oneOffVertical = [ { x = 2; y = 3 }; { x = 2; y = 4 } ]
    test <@ updateTail oneOffVertical = oneOffVertical @>

    let oneOffDiagonal = [ { x = 2; y = 3 }; { x = 1; y = 4 } ]
    test <@ updateTail oneOffDiagonal = oneOffDiagonal @>

    let twoOffHorizontal = [ { x = 3; y = 1 }; { x = 1; y = 1 } ]
    let twoOffHorizontalResult = [ twoOffHorizontal[0]; { x = 2; y = 1 } ]
    test <@ updateTail twoOffHorizontal = twoOffHorizontalResult @>

    let twoOffVertical = [ { x = 2; y = 3 }; { x = 2; y = 1 } ]
    let twoOffVerticalResult = [ twoOffVertical[0]; { x = 2; y = 2 } ]
    test <@ updateTail twoOffVertical = twoOffVerticalResult @>

    let twoOffDiagonal = [ { x = 2; y = 1 }; { x = 1; y = 3 } ]
    let twoOffDiagonalResult = [ twoOffDiagonal[0]; { x = 2; y = 2 } ]
    test <@ updateTail twoOffDiagonal = twoOffDiagonalResult @>

    let twoOffDiagonal2 = [ { x = 3; y = 2 }; { x = 1; y = 3 } ]
    let twoOffDiagonal2Result = [ twoOffDiagonal2[0]; { x = 2; y = 2 } ]
    test <@ updateTail twoOffDiagonal2 = twoOffDiagonal2Result @>

    test <@ motions |> simulateMotions 2 |> numberOfVisitedPositions = 13 @>

    let motionsLarge = readInput ("day09.test2.in")
    test <@ motionsLarge |> simulateMotions 10 |> numberOfVisitedPositions = 36 @>

//////////////////////////
// AoC
//////////////////////////
let motions = readInput ("day09.in")

motions
|> simulateMotions 2
|> numberOfVisitedPositions
|> printfn "The tail of the rope with 2 elements visited %d positions"

motions
|> simulateMotions 10
|> numberOfVisitedPositions
|> printfn "The tail of the rope with 10 elements visited %d positions"
