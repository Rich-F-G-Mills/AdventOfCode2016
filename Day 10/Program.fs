
open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


type Target =
    | Output of int
    | Bot of int

    override this.ToString () =
        match this with
        | Output idx -> $"output #{idx}"
        | Bot idx -> $"bot #{idx}"

[<DebuggerDisplay("{ToString()}")>]
type Instruction =
    | BotGives of BotIdx: int * LowTarget: Target * HighTarget: Target
    | BotReceives of BotIdx: int * Value: int

    override this.ToString () =
        match this with
        | BotGives (botIdx, lowTarget, highTarget) ->
            $"Bot #{botIdx} gives low value to {lowTarget} and high value to {highTarget}"
        | BotReceives (botIdx, value) ->
            $"Bot #{botIdx} recieves {value}"

type BotState =
    | SingleValue of Value: int
    | TwoValues of Low: int * High: int

type State =
    { BotStates: Map<int, BotState>
      OutputStates: Map<int, int> }


let (|ReMatch|_|) pattern =
    function
    | Regex.Match RegexOptions.None pattern ms -> Some ms
    | _ -> None

let parseTarget =
    function
    | "bot", idx -> Bot (int idx)
    | "output", idx -> Output (int idx)
    | _ -> failwith "Unexpected target."

let parseInstruction =
    function
    | ReMatch @"^value (\d+) goes to bot (\d+)$" { GroupValues = [ v; botIdx ] } ->
        BotReceives (int botIdx, int v)

    | ReMatch @"^bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)$"
        { GroupValues = [ botIdx; lowTargetType; lowValue; highTargetType; highValue ] } ->
            let lowTarget, highTarget =
                parseTarget (lowTargetType, lowValue), parseTarget (highTargetType, highValue)

            BotGives (int botIdx, lowTarget, highTarget)

    | line -> failwith $"Unable to parse '{line}'."


let determineOrdering instrs =
    let rec iterate ordered unordered toOrder =
        match toOrder with
        | (BotReceives _ as i)::is ->
            iterate (i::ordered) [] (unordered @ is)

        | (BotGives (idx, _, _) as i)::is ->
            ordered
            |> List.sumBy (function
                | BotGives (_, (Bot idx'), _) when idx = idx' -> 1
                | BotGives (_, _, (Bot idx')) when idx = idx' -> 1
                | BotReceives (idx', _) when idx = idx' -> 1
                | _ -> 0)
            |> function
                | 2 ->
                    iterate (i::ordered) [] (unordered @ is)
                | found when found > 2 ->
                    failwith "Unexpected error."
                | _ ->
                    iterate ordered (i::unordered) is

        | [] ->
            ordered |> List.rev

    iterate [] [] instrs


let sendValueToBot idx v ({ BotStates = botStates } as state)  =
    match botStates |> Map.tryFind idx with
    | None ->
        { state with
            BotStates = 
                botStates |> Map.add idx (SingleValue v) }

    | Some (SingleValue v') ->
        { state with
            BotStates =
                botStates |> Map.add idx (TwoValues (Math.Min (v, v'), Math.Max (v, v'))) }

    | Some (TwoValues _) ->
        failwith "Too many instructions for bot #{idx}."

let sendValueToOutput idx v ({ OutputStates = outputStates } as state)  =
    match outputStates |> Map.tryFind idx with
    | None ->
        { state with
            OutputStates = outputStates |> Map.add idx v }

    | Some _ ->
        failwith "Cannot assign more than one value to an output."


let updateStateForInstruction state =
    function
    | BotReceives (idx, v) ->
        sendValueToBot idx v state

    | BotGives (idx, lowerTarget, higherTarget) ->
        match state.BotStates |> Map.find idx with
        | TwoValues (low, high) ->
            let updaterForLow =
                match lowerTarget with
                | Output idx' ->
                    sendValueToOutput idx'
                | Bot idx' ->
                    sendValueToBot idx'

            let updaterForHigh =
                match higherTarget with
                | Output idx' ->
                    sendValueToOutput idx'
                | Bot idx' ->
                    sendValueToBot idx'

            state
            |> updaterForLow low
            |> updaterForHigh high

        | _ ->
            failwith "Bot cannot give anything until it has both chips."
        

[<EntryPoint>]
let main _ =

    let instructions =
        File.ReadAllLines "Inputs.txt"
        |> Array.map parseInstruction
        |> Array.toList
        |> determineOrdering

    let closingState =
        instructions
        |> List.fold updateStateForInstruction { BotStates = Map.empty; OutputStates = Map.empty }

    closingState.BotStates
    |> Map.toSeq
    |> Seq.find (function
        | _, TwoValues (low, high) when low = 17 && high = 61 -> true
        | _ -> false)
    |> fst
    |> printfn "Part 1 answer = %i\n"
    
    closingState.OutputStates
    |> Map.toSeq
    |> Seq.filter (fun (idx, _) -> idx <= 2)
    |> Seq.map snd
    |> Seq.reduce (*)
    |> printfn "Part 2 asnwer = %i"

    0