
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
        |> Seq.cast<Node>
        |> Seq.toArray        

    nodesFlattened
    |> Seq.allPairs nodesFlattened
    // Cannot pair up the same nodes.
    |> Seq.filter ((<||) (<>))
    // The first node is not empty and its usage fits within the second's availability.
    |> Seq.filter (fun ({ Used = used }, { Available = avail }) ->
        used > 0 && used <= avail)
    |> Seq.length
    |> printfn "Part 1 answer = %i\n"

    nodesFlattened
    |> Array.map (fun { X = x; Y = y; Size = size; Used = used; Available = avail } ->
        sprintf "%i,%i,%i,%i,%i" x y size used avail)
    |> fun data -> File.WriteAllLines("Nodes.txt", data)

    0