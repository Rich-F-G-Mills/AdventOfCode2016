
open System
open System.IO


[<EntryPoint>]
let main _ =
    let charCountsByColumn =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (Array.ofSeq >> Array.indexed)
        |> Array.concat
        |> Array.groupBy fst
        |> Array.sortBy fst
        |> Array.map (snd >> Array.map snd >> Array.countBy id >> Array.sortByDescending snd)

    charCountsByColumn
    |> Array.map (Array.head >> fst)
    |> String
    |> printfn "Part 1 answer = %s\n"

    charCountsByColumn
    |> Array.map (Array.last >> fst)
    |> String
    |> printfn "Part 2 answer = %s"

    0