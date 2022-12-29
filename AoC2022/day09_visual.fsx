#load "day09.fsx"

open Day09

type Rect =
    { left: int
      right: int
      top: int
      bottom: int }


let requiredRect (motions: Motion list) : Rect =
    let move (positions: Point list) motion =
        let head = positions[0]

        let newHead =
            match motion with
            | Right -> { x = head.x + 1; y = head.y }
            | Left -> { x = head.x - 1; y = head.y }
            | Up -> { x = head.x; y = head.y + 1 }
            | Down -> { x = head.x; y = head.y - 1 }

        newHead :: positions

    let positions = List.fold move [ { x = 0; y = 0 } ] motions
    let positionsX = List.map (fun p -> p.x) positions
    let positionsY = List.map (fun p -> p.y) positions

    { left = List.min positionsX
      right = List.max positionsX
      bottom = List.min positionsY
      top = List.max positionsY }


let drawRobe (width, height) robe =
    for y in 0 .. height - 1 do
        [ 0 .. width - 1 ]
        |> List.map (fun x -> if List.contains { x = x; y = y } robe then 'x' else '.')
        |> List.toArray
        |> System.String
        |> printfn "%s"


let animateRobe length motions =
    let rect = requiredRect motions
    let width = rect.right - rect.left + 1
    let height = rect.top - rect.bottom + 1

    let initialPos =
        { x = max 0 (-rect.left)
          y = max 0 (-rect.bottom) }

    let mutable rope = List.init length (fun _ -> initialPos)

    System.Console.Clear()
    drawRobe (width, height) rope

    for motion in motions do
        Async.Sleep(System.TimeSpan(0, 0, 0, 0, 100)) |> Async.RunSynchronously
        rope <- moveRope rope motion

        System.Console.Clear()
        drawRobe (width, height) rope


let motions = readInput ("day09.test2.in")
animateRobe 10 motions
