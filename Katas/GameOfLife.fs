module Katas.GameOfLife

let evolveCells width height aliveCells =
    let wrapBorderCell (x, y) =
        let newx =
            match x with
            | x when x < 1 -> width
            | x when x > width -> 1
            | _ -> x

        let newy =
            match y with
            | y when y < 1 -> height
            | y when y > height -> 1
            | _ -> y

        (newx, newy)

    let neighbors (x, y) =
        [
            (x - 1, y - 1)
            (x, y - 1)
            (x + 1, y - 1)
            (x + 1, y)
            (x - 1, y)
            (x - 1, y + 1)
            (x, y + 1)
            (x + 1, y + 1)
        ]
        |> List.map wrapBorderCell

    let numberOfAliveNeighbors (cell : int * int) : int =
        neighbors cell
        |> List.filter (fun cell -> Set.contains cell aliveCells)
        |> List.length

    let survivingCells =
        Set.filter
            (fun cell ->
                let n = numberOfAliveNeighbors cell

                n = 2 || n = 3
            )
            aliveCells

    let newCells =
        aliveCells
        |> Set.toList
        |> List.map neighbors
        |> List.concat
        |> Set.ofList
        |> Set.filter (fun cell -> numberOfAliveNeighbors cell = 3)

    survivingCells + newCells
