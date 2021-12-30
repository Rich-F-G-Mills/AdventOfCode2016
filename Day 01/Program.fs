
open System
open System.IO
open FSharpx


type Direction =
    | Left
    | Right
    | Up
    | Down

type State =
    { Direction: Direction
      X: int
      Y: int }

type DirectionChange =
    | LookLeft
    | LookRight

type Instruction =
    Instruction of DirChange: DirectionChange * Distance: int

let updateStateForInstruction state (Instruction (dirChange, distance)) =
    let newDirection =
        match state.Direction, dirChange with
        | Left, LookLeft -> Down
        | Left, LookRight -> Up
        | Right, LookLeft -> Up
        | Right, LookRight -> Down
        | Up, LookLeft -> Left
        | Up, LookRight -> Right
        | Down, LookLeft -> Right
        | Down, LookRight -> Left

    let (dX, dY) =
        match newDirection with
        | Left -> (-1, 0)
        | Right -> (1, 0)
        | Up -> (0, 1)
        | Down -> (0, -1)

    { state with
        Direction = newDirection
        X = state.X + dX * distance
        Y = state.Y + dY * distance }


[<EntryPoint>]
let main _ =

    let instructions =
        File.ReadAllText "Inputs.txt"
        |> String.splitString [|","|] StringSplitOptions.TrimEntries
        |> Array.map (fun str -> str.[0], int str.[1..])
        |> Array.map (function
            | 'L', dist -> Instruction (LookLeft, dist)
            | 'R', dist -> Instruction (LookRight, dist)
            | _ -> failwith "Unrecognised instruction.")

    let locationsVisited =
        instructions
        |> Array.scan updateStateForInstruction { Direction = Up; X = 0; Y = 0 }
        // Extract the coordinates from our accrued states.
        |> Array.map (fun { X = x; Y = y } -> (x, y))
        // Here we look at pairwise elements in order to generate all intermediate positions.
        |> Array.pairwise
        // Concatenate all of the generated intermediate positions into a single array.
        |> Array.collect (fun ((x1, y1), (x2, y2)) ->
            if x1 = x2 then
                let step =
                    if y2 > y1 then 1 else -1

                Array.map (fun y -> (x1, y)) [| (y1+step)..step..y2 |]
            else
                let step =
                    if x2 > x1 then 1 else -1

                Array.map (fun x -> (x, y1)) [| (x1+step)..step..x2 |])
        |> Array.append [| 0, 0 |]
        // We retain an index as needed for part 2.
        |> Array.indexed

    locationsVisited
    // For part 1, we only care about the final location visited.
    |> Array.last
    |> fun (_, (x, y)) -> Math.Abs(x) + Math.Abs(y)
    |> printfn "Part 1 answer = %i\n"

    locationsVisited
    // Use a sequence to make the finding of a duplicate lazy.
    |> Seq.find (fun (idx, l) ->
        Array.exists (fun (idx', l') ->
            // We're interested in the first element which has already occured earlier in the array.
            l = l' && idx > idx') locationsVisited)
    |> fun (_, (x, y)) -> Math.Abs(x) + Math.Abs(y)
    |> printfn "Part 2 answer = %i"

    0