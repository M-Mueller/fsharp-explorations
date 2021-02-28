namespace GameOfLife

module GameOfLife =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types
    open Avalonia.Layout
    open Avalonia.Threading
    open Elmish

    type State =
        { interval: float
          lastUpdate: System.DateTime
          aliveCells: Set<int * int>
          width: int
          height: int }

    let init =
        { interval = 1.0
          lastUpdate = System.DateTime.Now
          aliveCells = Set([])
          width = 40
          height = 40 }

    type Msg =
        | Reset
        | ChangeInterval of float
        | KillCell of int * int
        | ReviveCell of int * int
        | Tick of System.DateTime

    let subscription model =
        let sub dispatch =
            let action () =
                dispatch (Tick System.DateTime.Now)
                true

            DispatcherTimer.Run(System.Func<bool>(action), System.TimeSpan.FromMilliseconds 100.0)
            |> ignore

        Cmd.ofSub sub

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
            [ (x - 1, y - 1)
              (x, y - 1)
              (x + 1, y - 1)
              (x + 1, y)
              (x - 1, y)
              (x - 1, y + 1)
              (x, y + 1)
              (x + 1, y + 1) ]
            |> List.map wrapBorderCell

        let numberOfAliveNeighbors (cell: int * int) : int =
            neighbors cell
            |> List.filter (fun cell -> Set.contains cell aliveCells)
            |> List.length

        let survivingCells =
            Set.filter
                (fun cell ->
                    let n = numberOfAliveNeighbors cell

                    n = 2 || n = 3)
                aliveCells

        let newCells =
            aliveCells
            |> Set.toList
            |> List.map neighbors
            |> List.concat
            |> Set.ofList
            |> Set.filter (fun cell -> numberOfAliveNeighbors cell = 3)

        survivingCells + newCells


    let update (msg: Msg) (state: State) : State =
        match msg with
        | Reset -> init
        | ChangeInterval interval -> { state with interval = interval }
        | KillCell (row, col) ->
            { state with
                  aliveCells = Set.remove (row, col) state.aliveCells }
        | ReviveCell (row, col) ->
            { state with
                  aliveCells = Set.add (row, col) state.aliveCells }
        | Tick time ->
            let passedTime = time - state.lastUpdate

            let numberOfTicks =
                if state.interval = 0.0 then
                    0
                else
                    int (passedTime.TotalSeconds / state.interval)

            if numberOfTicks > 0 then
                { state with
                      aliveCells = evolveCells state.width state.height state.aliveCells
                      lastUpdate = time }
            else
                state

    let viewInterval interval dispatch =
        NumericUpDown.create [ NumericUpDown.onValueChanged (fun value -> dispatch (ChangeInterval value))
                               NumericUpDown.increment 0.5
                               NumericUpDown.minimum 0.0
                               NumericUpDown.maximum 5.0
                               NumericUpDown.value interval ]
        :> IView

    let viewCell isAlive row col dispatch =
        Button.create [ Button.onClick
                            (fun _ ->
                                if isAlive then
                                    dispatch (KillCell(row, col))
                                else
                                    dispatch (ReviveCell(row, col)))
                        Button.width 10.0
                        Button.height 10.0
                        Button.background (if isAlive then "Black" else "Gray") ]
        :> IView

    let view (state: State) (dispatch) =
        StackPanel.create [ StackPanel.orientation Orientation.Vertical
                            StackPanel.children (
                                (viewInterval state.interval dispatch)
                                :: ([ 1 .. state.height ]
                                    |> List.map
                                        (fun col ->
                                            StackPanel.create [ StackPanel.orientation Orientation.Horizontal
                                                                StackPanel.children (
                                                                    [ 1 .. state.width ]
                                                                    |> List.map
                                                                        (fun row ->
                                                                            viewCell
                                                                                (state.aliveCells.Contains(row, col))
                                                                                row
                                                                                col
                                                                                dispatch)
                                                                ) ]
                                            :> IView))
                            ) ]
