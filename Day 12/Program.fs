
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

type State =
    { Offset: int
      Registers: Map<string, int> }


let (|ReMatch|_|) pattern =
    function
    | Regex.Match RegexOptions.None pattern ms -> Some ms
    | _ -> None

let parseInstruction =
    function
    | ReMatch "^cpy (-?\d+) (\w)$" { GroupValues = [ v; tr ] } ->
        Cpy (Value <| int v, tr)
    | ReMatch "^cpy (\w) (\w)$" { GroupValues = [ sr; tr ] } ->
        Cpy (Register sr, tr)
    | ReMatch "^jnz (-?\d+) (-?\d+)$" { GroupValues = [ v; o ] } ->
        Jnz (Value <| int v, int o)
    | ReMatch "^jnz (\w) (-?\d+)$" { GroupValues = [ sr; o ] } ->
        Jnz (Register sr, int o)
    | ReMatch "^inc (\w)$" { GroupValues = [ tr ] } ->
        Inc tr
    | ReMatch "^dec (\w)$" { GroupValues = [ tr ] } ->
        Dec tr
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

    | Jnz (Value v, offsetDelta) when v <> 0 ->
        { state with Offset = offset + offsetDelta }

    | Jnz (Register sourceReg, offsetDelta) when registers.[sourceReg] <> 0 ->
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

let rec createStateIterator (instructions: Instruction array) state =
    seq {
        if state.Offset < instructions.Length then
            let newState =
                processInstruction state instructions.[state.Offset]

            yield newState
            yield! createStateIterator instructions newState
    }

let initState cValue =
    { Offset = 0
      Registers =
        ["a"; "b"; "c"; "d"]
        |> Seq.map (fun r -> r, (if r = "c" then cValue else 0))
        |> Map.ofSeq }
        
        
[<EntryPoint>]
let main _ =

    let instructions =
        File.ReadAllLines "Inputs.txt"
        |> Array.map parseInstruction

    let runInstructions =
        createStateIterator instructions
        >> Seq.last
        >> function | { Registers = registers } -> registers.["a"]

    initState 0
    |> runInstructions
    |> printfn "Part 1 answer = %i\n"

    initState 1
    |> runInstructions
    |> printfn "Part 2 answer = %i"

    0