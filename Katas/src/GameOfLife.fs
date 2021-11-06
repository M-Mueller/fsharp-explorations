module Katas.GameOfLife

let evolveCells width height aliveCells =
    let wrapBorderCell (x, y) =
        let newx =
            if x < 1 then width
            else if x > width then 1
            else x

        let newy =
            if y < 1 then height
            else if y > height then 1
            else y

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
