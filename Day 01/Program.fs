
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

    locationsVisited
    |> Array.last
    |> function
        | { X = x; Y = y } -> x + y
    |> printfn "Part 1 answer = %i"

    0