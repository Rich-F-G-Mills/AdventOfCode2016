
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


type Disk =
    { HoleCount: int
      Origin: int }


[<EntryPoint>]
let main _ =

    let disks =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (function
            | Regex.Match RegexOptions.None @"^Disc #(\d) has (\d+) positions; at time=0, it is at position (\d+)\.$"
                { GroupValues = [ idx; count; origin ] } ->
                    (int idx, { HoleCount = int count; Origin = int origin })
            | line ->
                failwith $"Unable to parse '{line}'.")
        |> Array.sortBy fst
        |> Array.map snd


    let solveForDisks disks =
        Seq.initInfinite (fun time ->
            disks
            |> Seq.mapi (fun idx disk ->
                (disk.Origin + time + idx + 1) % disk.HoleCount)
            |> Seq.reduce (+))
        |> Seq.findIndex ((=) 0)


    disks
    |> solveForDisks
    |> printfn "Part 1 answer = %i\n"

    Array.append disks [| { HoleCount = 11; Origin = 0 } |]
    |> solveForDisks
    |> printfn "Part 2 answer = %i"

    0