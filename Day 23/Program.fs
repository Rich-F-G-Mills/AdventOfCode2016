
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


type ValueOrRegister =
    | Value of int
    | Register of string

type Instruction =
    | Cpy of Source: ValueOrRegister * Register: string
    | Jnz of Source: ValueOrRegister * Offset: int
    | Inc of Register: string
    | Dec of Register: string


let (|Match'|_|) =
    Regex.(|Match|_|) RegexOptions.None


let parseInstruction =
    function
    | Match' "^cpy (-?\d+) (\w)$" { GroupValues = [ v; tr ] } ->
        Cpy (Value <| int v, tr)
    | Match' "^cpy (\w) (\w)$" { GroupValues = [ sr; tr ] } ->
        Cpy (Register sr, tr)
    | Match' "^jnz (-?\d+) (-?\d+)$" { GroupValues = [ v; o ] } ->
        Jnz (Value <| int v, int o)
    | Match' "^jnz (\w) (-?\d+)$" { GroupValues = [ sr; o ] } ->
        Jnz (Register sr, int o)
    | Match' "^inc (\w)$" { GroupValues = [ tr ] } ->
        Inc tr
    | Match' "^dec (\w)$" { GroupValues = [ tr ] } ->
        Dec tr
    | instr ->
        failwith $"Unable to parse '{instr}'."


[<EntryPoint>]
let main _ =

    let instructions =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (function
            | Match' @"^cpy "

    0