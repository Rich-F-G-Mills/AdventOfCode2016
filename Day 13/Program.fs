
open System
open System.Collections.Specialized


let [<Literal>] PuzzleInput = 1364


type CellType =
    | Wall
    | OpenSpace of MinDistance: int option

        
let determineCell x y =
    let binRep =
        BitVector32 (x * x + 3 * x + 2 * x * y + y + y * y + PuzzleInput)

    Seq.init 32 (fun idx -> binRep.[1 <<< idx])
    |> Seq.sumBy Convert.ToInt32
    |> function
        | sum when sum % 2 = 0 -> OpenSpace None
        | _ -> Wall


let calculateMinDistances grid =
    let width, height =
        Array2D.length1 grid, Array2D.length2 grid

    let surroundingOffsets =
        [| (-1, 0); (1, 0); (0, -1); (0, 1) |]

    let prevGrid =
        grid |> Array2D.copy

    let rec loop () =
        Array2D.blit grid 0 0 prevGrid 0 0 width height

        for y = 0 to (height-1) do
            for x = 0 to (width-1) do
                grid.[x, y] <-
                    match grid.[x, y] with
                    | OpenSpace minDistance as openSpace ->
                        let newMinDistance =
                            surroundingOffsets
                            |> Array.map (fun (dx, dy) -> (x + dx, y + dy))
                            |> Array.filter (fun (x', y') -> (x' >= 0) && (x' < width) && (y' >= 0) && (y' < height))
                            |> Array.choose (fun (x', y') ->
                                match grid.[x', y'] with
                                | OpenSpace (Some _ as minD) -> minD
                                | _ -> None)
                            |> function
                                | [||] -> None
                                | distances ->
                                    distances
                                    |> Array.min
                                    |> (+) 1
                                    |> Some

                        match minDistance, newMinDistance with
                        | Some minDistance', Some newMinDistance' when minDistance' >= newMinDistance' ->
                            OpenSpace newMinDistance
                        | None, Some _ ->
                            OpenSpace newMinDistance
                        | _ ->
                            openSpace

                    | wall -> wall

        (prevGrid |> Seq.cast)
        |> Seq.zip (grid |> Seq.cast)
        |> Seq.exists ((<||) (<>))
        |> function | true -> loop () | false -> ()

    loop ()


[<EntryPoint>]
let main _ =

    let grid =
        Array2D.init 50 50 determineCell

    grid.[1, 1] <- OpenSpace <| Some 0

    do calculateMinDistances grid

    grid.[31, 39]
    |> function
        | OpenSpace (Some minDistance) -> minDistance
        | _ -> failwith "Unexpected error."
    |> printfn "Part 1 answer = %i\n"

    grid
    |> Seq.cast
    |> Seq.sumBy (function
        | OpenSpace (Some minDistance) when minDistance <= 50 -> 1
        | _ -> 0)
    |> printfn "Part 2 answer = %i"
    
    0