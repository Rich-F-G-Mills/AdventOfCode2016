
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

type private VariantPresence =
    | GeneratorOnly of Variant: string
    | MicrochipOnly of Variant: string
    | GeneratorAndMicrochip of Variant: string

let log msg =
    printfn "%s" msg
    id


[<EntryPoint>]
let main _ =
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
        |> Seq.mapi (fun idx -> Seq.map (fun e -> (e, 1 + idx)))
        |> Seq.concat
        |> Map.ofSeq

    let distinctElements =
        startingState
        |> Map.keys
        |> List.ofSeq

    let allValidStates =
        let numCombinations =
            Seq.replicate (distinctElements.Length) NumFloors |> Seq.reduce (*)
            |> (+) (-1)

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
                    Some (GeneratorOnly v)
                | v, [ Microchip _ ] ->
                    Some (MicrochipOnly v)
                // We don't care about matching pairs of generators and their microchips.
                | v, [ Generator _; Microchip _ ]
                | v, [ Microchip _; Generator _ ] ->
                    Some (GeneratorAndMicrochip v)
                | _ ->
                    failwith "Unexpected combination for variant.")

        let isRowValid elems =
            let elementPairs =
                identifyElementPairs elems

            let hasOnlyGenerator =
                elementPairs
                |> List.exists (function | GeneratorOnly _ -> true | _ -> false)

            let hasOnlyMicrochip =
                elementPairs
                |> List.exists (function | MicrochipOnly _ -> true | _ -> false)

            not (hasOnlyGenerator && hasOnlyMicrochip)            

        Seq.init numCombinations rebaseNumber
        |> Seq.map (
            List.zip distinctElements
            >> List.groupBy snd
            >> Seq.map (fun (f, es) -> f, es |> List.map fst)
            >> Map.ofSeq)
        |> Seq.filter (Map.values >> Seq.forall isRowValid)
        |> Seq.toList

    printfn "Length = %i" (allValidStates.Length)

    0