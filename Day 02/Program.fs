
open System
open System.IO


type Direction =
    | Left
    | Right
    | Up
    | Down


let instructionToOffset =
    function
    | Left ->  (-1, 0)
    | Right -> (1, 0)
    | Up ->    (0, 1)
    | Down ->  (0, -1)

let instructionsToDigitPart1 =
    Array.map instructionToOffset
    >> Array.fold (fun (x, y) (dx, dy) ->
        (Math.Clamp(x + dx, 0, 2), Math.Clamp(y + dy, 0, 2))) (1, 1)
    >> fun (x, y) -> 1 + x + 3 * (2 - y)

let instructionsToDigitPart2 =
    Array.map instructionToOffset
    >> Array.fold (fun (x, y) (dx, dy) ->
        let (newX, newY) = (x + dx), (y + dy)
        if Math.Abs(newX) + Math.Abs(newY) > 2 then (x, y) else (newX, newY)) (-2, 0)
    >> function
       | 0, -2 -> "D"
       | -1, -1 -> "A" | 0, -1 -> "B" | 1, -1 -> "C"
       | -2, 0 -> "5" | -1, 0 -> "6" | 0, 0 -> "7" | 1, 0 -> "8" | 2, 0 -> "9"
       | -1, 1 -> "2" | 0, 1 -> "3" | 1, 1 -> "4"
       | 0, 2 -> "1"
       | _ -> failwith "Invalid key pad entry."


[<EntryPoint>]
let main _ =
    let instructions =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (
            Seq.map (function
                | 'L' -> Left
                | 'R' -> Right
                | 'U' -> Up
                | 'D' -> Down
                | _ -> failwith "Unknown instruction."))
        |> Array.map Seq.toArray

    instructions
    |> Array.map instructionsToDigitPart1
    |> Array.map (fun n -> n.ToString())
    |> String.concat String.Empty
    |> printfn "Part 1 answer = %s\n"

    instructions
    |> Array.map instructionsToDigitPart2
    |> String.concat String.Empty
    |> printfn "Part 2 answer = %s"

    0