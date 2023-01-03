#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System

type Point = { x: int; y: int }

type Input =
    { heightmap: int[,]
      start: Point
      finish: Point }

let parseInput filename =
    let input =
        IO.File.ReadLines(filename) |> Seq.toArray |> Array.map Seq.toArray |> array2D

    let mutable start = None
    let mutable finish = None

    let heightmap =
        input
        |> Array2D.mapi (fun y x v ->
            if v = 'S' then
                start <- Some({ y = y; x = x })
                'a'
            elif v = 'E' then
                finish <- Some({ y = y; x = x })
                'z'
            else
                v)
        |> Array2D.map (fun v -> int v - int 'a')

    assert (start <> None)
    assert (finish <> None)

    { heightmap = heightmap
      start = Option.get start
      finish = Option.get finish }


let neighbors (map: int[,]) (p: Point) =
    let rows = Array2D.length1 map
    let cols = Array2D.length2 map

    seq {
        if p.y > 0 then
            (p.y - 1, p.x)

        if p.y < rows - 1 then
            (p.y + 1, p.x)

        if p.x > 0 then
            (p.y, p.x - 1)

        if p.x < cols - 1 then
            (p.y, p.x + 1)
    }
    |> Seq.filter (fun (y, x) -> (map[y, x] - map[p.y, p.x]) <= 1)


let shortestPath map start finish =
    let height = Array2D.length1 map
    let width = Array2D.length2 map

    let visited: bool[,] = Array2D.init height width (fun _ _ -> false)
    let distances: int[,] = Array2D.init height width (fun _ _ -> Int32.MaxValue)
    let prevNode: Point option[,] = Array2D.init height width (fun _ _ -> None)

    distances[start.y, start.x] <- 0

    // Dijkstra's algorithm
    let rec processNode p =
        let unvisitedNeighbors =
            neighbors map p |> Seq.filter (fun (y, x) -> not visited[y, x])

        for (ny, nx) in unvisitedNeighbors do
            let distance = distances[p.y, p.x] + 1

            if distance < distances[ny, nx] then
                distances[ny, nx] <- distance
                prevNode[ny, nx] <- Some(p)

        visited[p.y, p.x] <- true

        let mutable nextNode = None
        let mutable smallestDist = Int32.MaxValue

        Array2D.iteri
            (fun y x _ ->
                if not visited[y, x] && distances[y, x] < smallestDist then
                    nextNode <- Some({ x = x; y = y })
                    smallestDist <- distances[y, x])
            distances

        if visited[finish.y, finish.x] then
            true
        else
            match nextNode with
            | Some next -> processNode next
            | None -> false

    if processNode start then
        assert (prevNode[finish.y, finish.x] <> None)

        let rec backtrackPath current =
            match prevNode[current.y, current.x] with
            | Some next -> current :: (backtrackPath next)
            | None -> start :: []

        Some(backtrackPath finish |> List.rev)

    else
        None


let drawPath map path =
    let height = Array2D.length1 map
    let width = Array2D.length2 map

    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let index = List.tryFindIndex (fun p -> p.x = x && p.y = y) path

            let symbol =
                match index with
                | Some index ->
                    match List.tryItem (index + 1) path with
                    | Some next when next.x < x -> '<'
                    | Some next when next.x > x -> '>'
                    | Some next when next.y > y -> 'v'
                    | Some next when next.y < y -> '^'
                    | None -> 'E'
                    | _ -> failwithf "Invalid path"
                | None -> '.'

            printf "%c" symbol

        printfn ""


let minimumRequiredSteps input =
    let path = shortestPath input.heightmap input.start input.finish

    match path with
    | Some p -> Some(List.length p - 1)
    | None -> None


let minimumRequiredStepsFromAnyStart input =
    let mutable starts = []

    Array2D.iteri
        (fun y x h ->
            if h = 0 then
                starts <- { y = y; x = x } :: starts)
        input.heightmap

    assert (starts <> [])

    starts
    |> List.choose (fun start -> minimumRequiredSteps { input with start = start })
    |> List.sort
    |> List.head


//////////////////////////
// Tests
//////////////////////////

let run_tests () =
    let input = parseInput "day12.test.in"

    test <@ input.start = { y = 0; x = 0 } @>
    test <@ input.finish = { y = 2; x = 5 } @>
    test <@ Array2D.length1 input.heightmap = 5 @>
    test <@ Array2D.length2 input.heightmap = 8 @>
    test <@ input.heightmap[0, 0] = 0 @>
    test <@ input.heightmap[4, 0] = 0 @>
    test <@ input.heightmap[0, 7] = 12 @>
    test <@ input.heightmap[4, 7] = 8 @>
    test <@ input.heightmap[2, 5] = 25 @>

    let path = shortestPath input.heightmap input.start input.finish
    test <@ path <> None @>
    let path = Option.get path

    test <@ List.head path = input.start @>
    test <@ List.last path = input.finish @>

    test <@ minimumRequiredSteps input = Some 31 @>

    test <@ minimumRequiredStepsFromAnyStart input = 29 @>


run_tests ()

//////////////////////////
// AoC
//////////////////////////
let input = parseInput "day12.in"

input
|> minimumRequiredSteps
|> Option.get
|> printfn "It requires %d steps to reach the goal"

input
|> minimumRequiredStepsFromAnyStart
|> printfn "It requires %d steps from any start to reach the goal"
