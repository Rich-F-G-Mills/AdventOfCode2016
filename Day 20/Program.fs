
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


let intersectRanges =
    function
    // No intersection at all.
    | (v1, v2) as valid, (i1, i2) when i1 > v2 || i2 < v1 ->
        [| valid |]
    // Whole range is invalid.
    | (v1, v2), (i1, i2) when i1 <= v1 && i2 >= v2 ->
        [||]
    // Lower part of range is invalid.
    | (v1, v2), (i1, i2) when i1 <= v1 && i2 >= v1 && i2 < v2 ->
        [| (i2 + 1u, v2) |]
    // Upper part of range is invalid.
    | (v1, v2), (i1, i2) when i1 > v1 && i1 <= v2 && i2 >= v2 ->
        [| (v1, i1 - 1u) |]
    // Interior part of range is invalid.
    | (v1, v2), (i1, i2) when i1 > v1 && i2 < v2 ->
        [| (v1, i1 - 1u); (i2 + 1u, v2) |]
    | (v1, v2), (i1, i2) ->
        failwith $"Unexpected combination: ({v1}, {v2}) ({i1}, {i2})"
    


[<EntryPoint>]
let main _ =
    
    let blockedIPs =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (function
            | Regex.Match RegexOptions.None @"^(\d+)-(\d+)$"
                { GroupValues = [ ip1; ip2 ] } ->
                    (uint ip1, uint ip2)
            | line ->
                failwith $"Unable to parse '{line}'.")
        |> Array.sortBy fst

    let validRanges =
        blockedIPs
        |> Array.fold (fun validRanges invalidRange ->
            validRanges
            |> Array.collect (fun validRange ->
                intersectRanges (validRange, invalidRange))) [| (0u, UInt32.MaxValue) |]
        |> Array.sortBy fst

    validRanges
    |> Array.head
    |> fst
    |> printfn "Part 1 answer = %i\n"

    validRanges
    |> Array.sumBy (fun (v1, v2) -> v2 + 1u - v1)
    |> printfn "Part 2 answer = %i"

    0