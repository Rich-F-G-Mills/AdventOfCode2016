
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


[<ReferenceEquality>]
type Node =
    { X: int
      Y: int
      Size: int
      Used: int
      Available: int }


let (|Match'|_|) =
    Regex.(|Match|_|) RegexOptions.None

let (|AsInt|_|) (str: string) =
    match Int32.TryParse str with
    | true, d -> Some d
    | false, _ -> None


[<EntryPoint>]
let main _ =

    // Stores the nodes in a 2D array.
    let nodes =
        let nodeData =
            File.ReadAllLines "Inputs.txt"
            |> Array.skip 2
            |> Array.map (function
                | Match' @"^/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+\d+%$"
                    { GroupValues = [ AsInt x; AsInt y; AsInt size; AsInt used; AsInt avail ] } ->
                        let newNode =
                            { X = x; Y = y; Size = size; Used = used; Available = avail }

                        (x, y), newNode

                | line -> failwith $"Unable to parse '{line}'.")

        let gridWidth, gridHeight =
            let coords =
                nodeData
                |> Array.map fst

            1 + (coords |> Array.map fst |> Array.max),
            1 + (coords |> Array.map snd |> Array.max)

        let nodeDataMap =
            nodeData |> Map.ofArray

        Array2D.init gridWidth gridHeight (fun x y ->
            nodeDataMap.[x, y])

    let nodesFlattened =
        nodes
        |> Seq.cast
        |> Seq.toArray        

    nodesFlattened
    |> Seq.allPairs nodesFlattened
    // Cannot pair up the same nodes.
    |> Seq.filter ((<||) (<>))
    // The first node is not empty and its usage fits within the second's availability.
    |> Seq.filter (fun ({ Used = thisUsed }, { Available = otherAvail }) ->
        thisUsed > 0 && thisUsed <= otherAvail)
    |> Seq.toArray
    |> Array.length
    |> printfn "Part 1 answer = %i\n"

    let distances =
        let adjacentOffsets =
            Array.allPairs [| -1; 0; 1 |] [| -1; 0; 1 |]
            |> Array.filter (fun (dx, dy) -> Math.Abs (dx + dy) = 1)

        let startingDistances =
            nodes
            |> Array2D.map (function
                | { Used = 0 } -> Some 0
                | _ -> None)

        let gridWidth, gridHeight =
            nodes |> Array2D.length1,
            nodes |> Array2D.length2

        let iteratedDistances =
            Seq.unfold (fun distances ->
                let newDistMatrix =
                    distances
                    |> Array2D.mapi (fun x y minDist ->
                        let nodeFrom =
                            nodes.[x, y]

                        let minAdjacentDist =
                            adjacentOffsets
                            |> Array.filter (fun (dx, dy) ->
                                x+dx >= 0 && x+dx < gridWidth && y+dy >= 0 && y+dy < gridHeight)
                            |> Array.choose (fun (dx, dy) ->
                                let nodeTo =
                                    nodes.[x + dx, y + dy]

                                if nodeTo.Size > nodeFrom.Used then
                                    distances.[x + dx, y + dy]
                                else
                                    None)
                            |> function
                                | [||] -> None
                                | dists -> Some <| Array.min dists

                        match minDist, minAdjacentDist with
                        | None, None -> None
                        | None, Some mAD -> Some (mAD + 1)
                        | Some mD, Some mAD when mAD + 1 < mD -> Some (mAD + 1)
                        | Some _, _ -> minDist)

                if distances <> newDistMatrix then
                    Some (newDistMatrix, newDistMatrix)
                else
                    None) startingDistances

        iteratedDistances
        |> Seq.last

    distances
    |> Array2D.mapi (fun x y -> function
        | Some d -> $"{x},{y},{d}"
        | None -> $"{x},{y},X")
    |> Seq.cast
    |> Seq.toArray
    |> fun lines -> File.WriteAllLines ("Distances.txt", lines)  
    
    

    0