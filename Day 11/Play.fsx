
#r "nuget:FSharpx.Extras"

open System
open System.Text.RegularExpressions
open FSharpx
open FSharpx.IO
open FSharpx.Text

let [<Literal>] NumFloors = 4

type Element =
    | Generator of Variant: string
    | Microchip of Variant: string

    static member Name = function
        | Generator v -> v
        | Microchip v -> v

type ElevatorDirection =
    | Up
    | Down

    
type State =
    private State of Elevator: int * Elements: Map<Element, int>

type VariantPresence =
    | IsolatedGenerator of Variant: string
    | IsolatedMicrochip of Variant: string
    | GeneratorAndMicrochip of Variant: string

let startingState =
    let parseInputLine =
        let (|ElementMatch|_|) =
            function
            | Regex.Match RegexOptions.None @"([a-z]+)\s(generator|microchip)" ms ->
                Some ms.GroupValues
            | _ ->
                None

        fun str ->
            Regex.Matches(str, @"([a-z]+\s(?:generator|microchip))+")
            |> Seq.collect (fun m -> m.Captures |> Seq.map (fun c -> c.Value))
            |> Seq.map (function
                | ElementMatch [v; "generator"] -> Generator v
                | ElementMatch [v; "microchip"] -> Microchip v
                | elem -> failwith $"Unable to parse {elem}.")
            |> Seq.toList

    readFile "Inputs.txt"
    |> Seq.map (String.replace' "-compatible" String.Empty)
    |> Seq.map parseInputLine
    |> Seq.mapi (fun idx -> Seq.map (fun e -> (1 + idx, e)))
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map (fun (idx, es) -> (idx, es |> Seq.map snd |> Seq.toList))
    |> Map.ofSeq

let distinctElements =
    startingState
    |> Map.values
    |> Seq.concat
    |> List.ofSeq

let allValidStates =
    let numCombinations =
        Seq.replicate (distinctElements.Length) NumFloors |> Seq.reduce (*)

    let rebaseNumber =
        let units =
            Seq.initInfinite id
            |> Seq.scan (fun acc _ -> NumFloors * acc) 1
            |> Seq.take 10
            |> Seq.rev
            |> Seq.toList

        let rec loop =
            function
            | 0, [] -> Seq.empty        
            | _, [] -> failwith "Unable to convert %i to base 4."
            | cn, u::us ->             
                seq {
                    yield cn / u
                    yield! loop (cn % u, us)
                }

        fun n ->
            loop (n, units)
            |> Seq.toList

    let identifyElementPairs elems =
        elems
        |> List.groupBy Element.Name
        |> List.choose (function
            | v, [ Generator _ ] ->
                Some (IsolatedGenerator v)
            | v, [ Microchip _ ] ->
                Some (IsolatedMicrochip v)
            // We don't care about matching pairs of generators and their microchips.
            | v, [ Generator _; Microchip _ ]
            | v, [ Microchip _; Generator _ ] ->
                Some (GeneratorAndMicrochip v)
            | _ ->
                failwith "Unexpected combination for variant.")

    let isRowValid elems =
        let elementPairs =
            identifyElementPairs elems

        let hasIsolatedGenerator =
            elementPairs
            |> List.exists (function | IsolatedGenerator _ -> true | _ -> false)

        let hasIsolatedMicrochip =
            elementPairs
            |> List.exists (function | IsolatedMicrochip _ -> true | _ -> false)

        let hasPair =
            elementPairs
            |> List.exists (function | GeneratorAndMicrochip _ -> true | _ -> false)

        // Just because a generator has been paired with a microchip...
        // It can still irradiate other isolated chips.
        // However, a paired microchip is protected.
        not ((hasPair || hasIsolatedGenerator) && hasIsolatedMicrochip)

    Seq.init numCombinations rebaseNumber
    |> Seq.map (
        List.zip distinctElements
        >> List.groupBy snd
        >> Seq.map (fun (f, es) -> f, es |> List.map fst)
        >> Map.ofSeq)
    |> Seq.filter (Map.values >> Seq.forall isRowValid)
    |> Seq.toList

let groupedFloorUsage =
    let lengthsByFloor state idx =
        state
        |> Map.tryFind idx
        |> Option.defaultValue List.empty
        |> List.length
        
    allValidStates
    |> List.groupBy (lengthsByFloor >> Array.init 4)

let getAdjacentFloorUsage elevator currentUsage =
    let upperMoveOne =
        if currentUsage.[elevator] 
        currentUsage |> Array.mapi
