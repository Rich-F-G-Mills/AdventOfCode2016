
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


type Instruction =
    | SwapPositions of Position1: int * Position2: int
    | SwapLetters of Letter1: char * Letter2: char
    | MovePosition of From: int * To: int
    | ReversePositions of From: int * To: int
    | RotateBasedOnLetter of Letter: char
    | RotateLeft of Steps: int
    | RotateRight of Steps: int


let (|Match'|_|) =
    Regex.(|Match|_|) RegexOptions.None

let (|AsInt|_|) (str: string) =
    match Int32.TryParse (str) with
    | true, i -> Some i
    | false, _ -> None

let (|AsChar|_|) (str: string) =
    match Char.TryParse (str) with
    | true, i -> Some i
    | false, _ -> None

// Needed because remainders of negative numbers remain negative.
let inline modulo a b =
    if a >= 0 then a % b
    else (b + a % b) % b
    

let parseInstruction =
    function
    | Match' @"^swap position (\d+) with position (\d+)$"
        { GroupValues = [ AsInt pos1; AsInt pos2 ] } ->
            SwapPositions (pos1, pos2)
    | Match' @"^swap letter ([a-z]) with letter ([a-z])$"
        { GroupValues = [ AsChar letter1; AsChar letter2 ] } ->
            SwapLetters (letter1, letter2)
    | Match' @"^move position (\d+) to position (\d+)$"
        { GroupValues = [ AsInt posFrom; AsInt posTo ] } ->
            MovePosition (posFrom, posTo)
    | Match' @"^reverse positions (\d+) through (\d+)$"
        { GroupValues = [ AsInt posFrom; AsInt posTo ] } ->
            ReversePositions (posFrom, posTo)
    | Match' @"^rotate based on position of letter (\w)$"
        { GroupValues = [ AsChar letter ] } ->
            RotateBasedOnLetter letter
    | Match' @"^rotate left (\d+) steps?$"
        { GroupValues = [ AsInt steps ] } ->
            RotateLeft steps
    | Match' @"^rotate right (\d+) steps?$"
        { GroupValues = [ AsInt steps ] } ->
            RotateRight steps
    | instr -> failwith $"Unable to parse '{instr}'."

let rec processInstruction (chars: char []) =
    function
    | SwapPositions (p1, p2) ->
        chars
        |> Array.mapi (fun idx c ->
            if idx = p1 then chars.[p2]
            elif idx = p2 then chars.[p1]
            else c)

    | SwapLetters (c1, c2) ->
        chars
        |> Array.map (fun c ->
            if c = c1 then c2
            elif c = c2 then c1
            else c)

    | MovePosition (p1, p2) when p2 > p1 ->
        Array.init chars.Length (fun idx ->
            if idx >= p1 && idx < p2 then chars.[idx+1]
            elif idx = p2 then chars.[p1]
            else chars.[idx])

    | MovePosition (p1, p2) when p2 < p1 ->
        Array.init chars.Length (fun idx ->
            if idx > p2 && idx <= p1 then chars.[idx-1]
            elif idx = p2 then chars.[p1]
            else chars.[idx])   
            
    | ReversePositions (p1, p2) ->
        Array.init chars.Length (fun idx ->            
            if idx >= p1 && idx <= p2 then chars.[p2 - (idx - p1)]
            else chars.[idx])

    | RotateBasedOnLetter letter ->
        let letterIdx =
            chars |> Array.findIndex ((=) letter)

        let steps =
            1 + letterIdx + if letterIdx >= 4 then 1 else 0

        processInstruction chars (RotateRight steps)

    | RotateLeft steps ->
        Array.init chars.Length (fun idx ->
            chars.[modulo (idx + steps) chars.Length])

    | RotateRight steps ->
        Array.init chars.Length (fun idx ->
            chars.[modulo (idx - steps) chars.Length])

    | instr ->
        failwith $"Unexpected instruction '{instr}'."

let processInverseOfInstruction (chars: char []) =
    function
    | SwapPositions _
    | SwapLetters _
    | ReversePositions _ as instr ->
        processInstruction chars instr

    | MovePosition (p1, p2) ->
        processInstruction chars (MovePosition (p2, p1))            
    
    | RotateBasedOnLetter letter ->
        let letterIdx =
            chars |> Array.findIndex ((=) letter)

        let origIdx =
            seq { 0..(chars.Length-1) }
            // Which of these give the current letter location?...
            |> Seq.filter (fun idx ->
                letterIdx = (1 + 2 * idx + if idx >= 4 then 1 else 0) % chars.Length)
            // In some cases, two original locations may map to the current location.
            |> Seq.exactlyOne

        processInstruction chars (RotateLeft (letterIdx - origIdx))

    | RotateLeft steps ->
        processInstruction chars (RotateRight steps)

    | RotateRight steps ->
        processInstruction chars (RotateLeft steps)


[<EntryPoint>]
let main _ =

    let instructions =
        File.ReadAllLines "Inputs.txt"
        |> Array.map parseInstruction

    instructions
    |> Array.fold processInstruction ("abcdefgh" |> Array.ofSeq)
    |> String
    |> printfn "Part 1 answer = %s\n"

    instructions
    |> Array.rev
    |> Array.fold processInverseOfInstruction ("fbgdceah" |> Array.ofSeq)
    |> String
    |> printfn "Part 2 answer = %s"    

    0