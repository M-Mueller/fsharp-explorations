#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote
open System
open System.Text.RegularExpressions


type TargetArea =
    { xmin: int
      xmax: int
      ymin: int
      ymax: int }


type Probe =
    { x: int
      y: int
      vx: int
      vy: int }
    static member launch(vx, vy) = { x = 0; y = 0; vx = vx; vy = vy }


let parseInput line =
    let rx =
        Regex("""target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)""")

    let m = rx.Match(line)

    if m.Success then
        { xmin = int (m.Groups.[1].Value)
          xmax = int (m.Groups.[2].Value)
          ymin = int (m.Groups.[3].Value)
          ymax = int (m.Groups.[4].Value) }
    else
        failwithf "Invalid input: %s" line


let highestYPosition targetArea =
    let vy = targetArea.ymin

    vy * (vy + 1) / 2


let rec hitsTargetArea targetArea probe =
    let insideTargetArea (x, y) =
        x >= targetArea.xmin
        && x <= targetArea.xmax
        && y >= targetArea.ymin
        && y <= targetArea.ymax

    let belowTargetArea (x, y) =
        x > targetArea.xmax || y < targetArea.ymin

    if insideTargetArea (probe.x, probe.y) then
        true
    else if belowTargetArea (probe.x, probe.y) then
        false
    else
        let probe =
            { x = probe.x + probe.vx
              y = probe.y + probe.vy
              vx = max 0 (probe.vx - 1)
              vy = probe.vy - 1 }

        hitsTargetArea targetArea probe


let allHittingVelocities targetArea =
    let mutable probes = Set.empty<int * int>
    for x in 1..targetArea.xmax do
        for y in targetArea.ymin..(-targetArea.ymin) do
            if hitsTargetArea targetArea (Probe.launch (x, y)) then
                probes <- Set.add (x, y) probes
    probes


//////////////////////////
/// Tests
//////////////////////////

let runTests =
    let sampleTargetArea =
        parseInput "target area: x=20..30, y=-10..-5"

    test
        <@ sampleTargetArea = { xmin = 20
                                xmax = 30
                                ymin = -10
                                ymax = -5 } @>

    test <@ highestYPosition sampleTargetArea = 45 @>

    test <@ hitsTargetArea sampleTargetArea (Probe.launch (7, 2)) = true @>
    test <@ hitsTargetArea sampleTargetArea (Probe.launch (6, 3)) = true @>
    test <@ hitsTargetArea sampleTargetArea (Probe.launch (9, 0)) = true @>
    test <@ hitsTargetArea sampleTargetArea (Probe.launch (17, -4)) = false @>

    test <@ allHittingVelocities sampleTargetArea |> Set.count = 112 @>

//////////////////////////
/// AoC
//////////////////////////
"target area: x=29..73, y=-248..-194"
|> parseInput
|> highestYPosition
|> printfn "Highest possible y position is %d"

"target area: x=29..73, y=-248..-194"
|> parseInput
|> allHittingVelocities
|> Set.count
|> printfn "There are %d initial velocities that hit"
