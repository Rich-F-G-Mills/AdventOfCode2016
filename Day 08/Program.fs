
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


type Instruction =
    | CreateRect of Width: int * Height: int
    | RotateRow of Y: int * By: int
    | RotateCol of X: int * By: int


let (|IsCreateRect|_|) =
    function
    | Regex.Match RegexOptions.None @"^rect\s(\d+)x(\d+)$"
        { GroupValues = [ w; h ] } -> Some <| CreateRect (int w, int h)
    | _ -> None

let (|IsRotateRow|_|) =
    function
    | Regex.Match RegexOptions.None @"^rotate\srow\sy=(\d+)\sby\s(\d+)$"
        { GroupValues = [ y; by ] } -> Some <| RotateRow (int y, int by)
    | _ -> None

let (|IsRotateCol|_|) =
    function
    | Regex.Match RegexOptions.None @"^rotate\scolumn\sx=(\d+)\sby\s(\d+)$"
        { GroupValues = [ x; by ] } -> Some <| RotateCol (int x, int by)
    | _ -> None

let processInstruction (grid: bool [][]) =
    function
    | CreateRect (width, height) ->
        for r in 0..height-1 do
            for c in 0..width-1 do
                grid.[r].[c] <- true

    | RotateRow (y, by) ->
        let currentRow =
            grid.[y] |> Array.copy

        for c in 0..(currentRow.Length-1) do
            grid.[y].[(c + by) % currentRow.Length] <- currentRow.[c]

    | RotateCol (x, by) ->
        let currentCol =
            grid |> Array.map (fun row -> row.[x])

        for r in 0..(grid.Length-1) do
            grid.[(r + by) % grid.Length].[x] <- currentCol.[r]
                

[<EntryPoint>]
let main _ =
    
    let instructions =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (function
            | IsCreateRect instr | IsRotateRow instr | IsRotateCol instr -> instr
            | str -> failwith $"Unrecognised instruction '{str}'")

    let grid =
        Array.init 6 (fun _ -> Array.create 50 false)

    instructions
    |> Array.iter (processInstruction grid)

    grid
    |> Array.concat
    |> Array.sumBy (Convert.ToInt32)
    |> printfn "Part 1 answer = %i\n\n"

    printfn "Read the following for part 2:\n"

    grid
    |> Array.map (Array.map (fun v -> if v then '#' else ' '))
    |> Array.map String
    |> Array.iter (printfn "%s")

    0