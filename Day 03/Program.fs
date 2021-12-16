
open System
open System.IO
open FSharpx

// This logic is supplied by the puzzle.
let isValidTriangle =
    function
    | [| l1; l2; l3 |] ->
        l1 + l2 > l3 && l2 + l3 > l1 && l1 + l3 > l2
    | _ -> failwith "Invalid lengths."


[<EntryPoint>]
let main _ =

    let lengths =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (String.splitString [| " " |] StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (Array.map int)        

    lengths
    |> Array.filter isValidTriangle
    |> Array.length
    |> printfn "Part 1 answer = %i\n"
    
    // For part 2, we assign each length an index to say which row/column in came from.
    // Once we have that, we can group as necessary.
    lengths
    |> Array.map (Array.indexed)
    |> Array.indexed
    |> Array.collect (fun (r, ls) -> ls |> Array.map (fun (c, l) -> (r, c, l)))
    |> Array.sortBy (fun (r, c, _) -> (c, r))
    |> Array.map (fun (_, _, l) -> l)
    |> Array.chunkBySize 3
    |> Array.filter isValidTriangle
    |> Array.length
    |> printfn "Part 2 answer = %i"

    0