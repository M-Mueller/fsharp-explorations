let numLargerThanPrevious (measurements: int seq) =
    measurements
    |> Seq.pairwise
    |> Seq.filter (fun (a, b) -> a < b)
    |> Seq.length

let sumSlidingWindows (measurements: int seq) =
    measurements |> Seq.windowed 3 |> Seq.map Array.sum

let measurements =
    System.IO.File.ReadLines("day01.in")
    |> Seq.map int

printfn
    "Of %d values, %d are larger than their previous value"
    (Seq.length measurements)
    (numLargerThanPrevious measurements)

printfn
    "Of %d values, %d are larger than their previous value (with a sliding window of 3)"
    (Seq.length measurements)
    (measurements
     |> sumSlidingWindows
     |> numLargerThanPrevious)
