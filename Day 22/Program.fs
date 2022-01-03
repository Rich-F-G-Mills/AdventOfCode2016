
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text

type Node =
    { Size: decimal
      Used: decimal
      Available: decimal
      ``Used%``: decimal }


let (|Match'|_|) =
    Regex.(|Match|_|) RegexOptions.None

let (|AsInt|_|) (str: string) =
    match Int32.TryParse str with
    | true, d -> Some d
    | false, _ -> None

let (|AsDec|_|) (str: string) =
    match Decimal.TryParse str with
    | true, d -> Some d
    | false, _ -> None


[<EntryPoint>]
let main _ =

    let nodes =
        let nodeData =
            File.ReadAllLines "Inputs.txt"
            |> Array.skip 2
            |> Array.map (function
                | Match' @"^/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%$"
                    { GroupValues = [ AsInt x; AsInt y; AsDec size; AsDec used; AsDec avail; AsDec usedPc ] } ->
                        (x, y), { Size = size; Used = used; Available = avail; ``Used%`` = usedPc }
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
            nodeDataMap.[(x, y)])
            
    0