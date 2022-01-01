
open System.IO


type Tile =
    | Safe
    | Trap


[<EntryPoint>]
let main _ =
    
    let initRow =
        File.ReadAllText "Inputs.txt"
        |> Array.ofSeq
        |> Array.map (function
            | '.' -> Safe
            | '^' -> Trap
            | sym -> failwith $"Unable to parse '{sym}'.")

    let floorTiles =
        Seq.initInfinite id
        |> Seq.scan (fun state _ ->            
            let rowInterior =
                Array.concat [| [| Safe |]; state; [| Safe |] |]

            rowInterior
            |> Array.windowed 3
            |> Array.map (function
                | [| Trap; Trap; Safe |]
                | [| Safe; Trap; Trap |]
                | [| Trap; Safe; Safe |]
                | [| Safe; Safe; Trap |] -> Trap
                | _ -> Safe)) initRow

    floorTiles
    |> Seq.take 40
    |> Seq.concat
    |> Seq.sumBy (function | Safe -> 1 | _ -> 0)
    |> printfn "Part 1 answer = %i\n"

    floorTiles
    |> Seq.take 400_000
    |> Seq.concat
    |> Seq.sumBy (function | Safe -> 1 | _ -> 0)
    |> printfn "Part 2 answer = %i"

    0