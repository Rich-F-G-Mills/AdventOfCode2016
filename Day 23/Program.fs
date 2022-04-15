
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


type ValueOrRegister =
    | Value of int
    | Register of string

type Instruction =
    | Cpy of Source: ValueOrRegister * Destination: ValueOrRegister
    | Jnz of Source: ValueOrRegister * Offset: ValueOrRegister
    | Inc of Register: string
    | Dec of Register: string
    | Tgl of Register: string

type State =
    { Offset: int
      Registers: Map<string, int>
      Instructions: Map<int, Instruction> }


let (|Match'|_|) =
    Regex.(|Match|_|) RegexOptions.None


let parseInstruction =
    function
    | Match' "^cpy (-?\d+) (\w)$" { GroupValues = [ v; tr ] } ->
        Cpy (Value <| int v, tr)
    | Match' "^cpy (\w) (\w)$" { GroupValues = [ sr; tr ] } ->
        Cpy (Register sr, tr)
    | Match' "^jnz (-?\d+) (-?\d+)$" { GroupValues = [ v; o ] } ->
        Jnz (Value <| int v, Value <| int o)
    | Match' "^jnz (\w) (-?\d+)$" { GroupValues = [ sr; o ] } ->
        Jnz (Register sr, Value <| int o)
    | Match' "^jnz (-?\d+) (\w)$" { GroupValues = [ sr; o ] } ->
        Jnz (Value <| int sr, Register o)
    | Match' "^jnz (\w) (\w)$" { GroupValues = [ sr; o ] } ->
        Jnz (Register sr, Register o)
    | Match' "^inc (\w)$" { GroupValues = [ tr ] } ->
        Inc tr
    | Match' "^dec (\w)$" { GroupValues = [ tr ] } ->
        Dec tr
    | Match' "^tgl (\w)$" { GroupValues = [ tr ] } ->
        Tgl tr
    | instr ->
        failwith $"Unable to parse '{instr}'."


let processInstruction ({ Offset = offset; Registers = registers } as state) =
    function
    | Cpy (Value v, targetReg) ->
        { state with
            Offset = offset + 1
            Registers = registers |> Map.add targetReg v }
        
    | Cpy (Register sourceReg, targetReg) ->        
        { state with
            Offset = offset + 1
            Registers = registers |> Map.add targetReg (registers.[sourceReg]) }

    | Jnz (Value v, Value offsetDelta) when v <> 0 ->
        { state with Offset = offset + offsetDelta }

    | Jnz (Register sourceReg, Register offsetDelta) when registers.[sourceReg] <> 0 ->
        { state with Offset = offset + registers.[offsetDelta] }

    | Jnz (Value v, Register offsetDelta) when v <> 0 ->
        { state with Offset = offset + registers.[offsetDelta] }

    | Jnz (Register sourceReg, Value offsetDelta) when registers.[sourceReg] <> 0 ->
        { state with Offset = offset + offsetDelta }

    | Jnz _ ->
        { state with Offset = offset + 1 }

    | Inc targetReg ->
        { state with
            Offset = offset + 1
            Registers = registers |> Map.add targetReg (registers.[targetReg] + 1) }

    | Dec targetReg ->
        { state with
            Offset = offset + 1
            Registers = registers |> Map.add targetReg (registers.[targetReg] - 1) }


[<EntryPoint>]
let main _ =

    let instructions =
        File.ReadAllLines "Inputs.txt"
        |> Array.map parseInstruction

    0