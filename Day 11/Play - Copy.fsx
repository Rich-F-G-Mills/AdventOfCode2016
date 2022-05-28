﻿
#r @"C:/Users/Millch/.nuget/packages/fsharp.control.asyncseq/2.0.21/lib/netstandard2.0/FSharp.Control.AsyncSeq.dll"
#r @"C:/Users/Millch/.nuget/packages/fsharpx.async/1.14.1/lib/netstandard2.0/FSharpx.Async.dll"
#r @"C:/Users/Millch/.nuget/packages/fsharpx.collections/2.1.2/lib/netstandard2.0/FSharpx.Collections.dll"
#r @"C:/Users/Millch/.nuget/packages/fsharpx.extras/3.1.0/lib/netstandard2.0/FSharpx.Extras.dll"

open System
open System.Collections.Generic
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

type Floor = int

type ElevatorLocation = Floor

type ElementLayout =
    { Generators: Floor array
      Microchips: Floor array }

type State = ElementLayout * ElevatorLocation

let part1VariantNames, part1StartingState: String array * State =
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

    let startingLocations : (Element * Floor) list =
        readFile "Inputs.txt"
        |> Seq.map (String.replace' "-compatible" String.Empty)
        |> Seq.map parseInputLine
        |> Seq.mapi (fun idx -> Seq.map (fun e -> (e, 1+idx)))
        |> Seq.concat
        |> Seq.toList

    let variantNames =
        startingLocations
        |> Seq.map fst
        |> Seq.map Element.Name
        |> Seq.distinct
        |> Seq.sort
        |> Seq.toArray

    let generators, microchips =
        startingLocations
        |> Seq.map (function
            | Generator name, floor -> (0, name, floor)
            | Microchip name, floor -> (1, name, floor))
        |> Seq.sortBy (fun (l1, name, _) ->
            (l1, variantNames |> Array.findIndex ((=) name)))
        |> Seq.map (fun (_, _, floor) -> floor)
        |> Seq.toArray
        |> Array.splitAt (variantNames.Length)

    variantNames, ({ Generators = generators; Microchips = microchips }, 1)

let getTargetState (variantNames: string array) : State =
    let allTopFloor =
        Array.replicate (variantNames.Length) NumFloors

    { Generators = allTopFloor; Microchips = allTopFloor }, 4

let isValidLayout { Generators = generators; Microchips = microchips } =
    let zipped = Seq.zip generators microchips

    let isFloorValid floor =
        let hasPair =
            zipped |> Seq.exists (fun (g, m) -> g = m && m = floor)

        let hasIsolatedMicrochip =
            zipped |> Seq.exists (fun (g, m) -> g <> m && m = floor)

        let hasIsolatedGenerator =
            zipped |> Seq.exists (fun (g, m) -> g <> m && g = floor)

        not ((hasPair || hasIsolatedGenerator) && hasIsolatedMicrochip)

    Seq.init NumFloors ((+) 1)
    |> Seq.forall isFloorValid

let getAllValidLayouts numVariants: ElementLayout list =
    let numCombinations =
        Seq.replicate numVariants NumFloors |> Seq.reduce (*)

    let rebaseNumber =
        let units =
            Seq.initInfinite id
            |> Seq.scan (fun acc _ -> NumFloors * acc) 1
            |> Seq.take numVariants
            |> Seq.rev
            |> Seq.toList

        let rec loop =
            function
            | 0, [] -> Seq.empty        
            | _, [] -> failwith "Unable to convert %i to base 4."
            | cn, u::us ->             
                seq {
                    yield 1 + cn / u
                    yield! loop (cn % u, us)
                }

        fun n ->
            loop (n, units)
            |> Seq.toArray

    let floorCombinations =
        List.init numCombinations rebaseNumber

    floorCombinations
    |> Seq.allPairs floorCombinations
    |> Seq.map (fun (g, m) -> { Generators = g; Microchips = m})
    |> Seq.filter isValidLayout
    |> Seq.toList

let getLayoutsByFloorCounts =
    List.groupBy (fun { Generators = g; Microchips = m } ->
        Array.init NumFloors (fun idx ->
            let generatorsOnFloor =
                g |> Array.sumBy (function | f when f = idx + 1 -> 1 | _ -> 0)

            let microchipsOnFloor =
                m |> Array.sumBy (function | f when f = idx + 1 -> 1 | _ -> 0)

            generatorsOnFloor + microchipsOnFloor))
    >> Map.ofList

let getAdjacentFloorCounts =
    let adjustFloor (counts: int []) (floor: Floor) (amt: int) =
        if floor < 4 && counts[floor - 1] >= amt then
            Some (counts |> Array.mapi (fun idx c ->
                if idx = floor - 1 then c - amt
                elif idx = floor then c + amt
                else c))
        else
            None

    let getAdjacentsForCounts (counts: int []) =
        [1..3]
        |> Seq.map (fun floor ->
            let adjacentCounts =
                [1..2] |> List.choose (adjustFloor counts floor)

            (floor, adjacentCounts))
        |> Map.ofSeq

    Seq.map getAdjacentsForCounts
        
let allValidUpMovements numVariants layoutsByFloorCounts adjacentFloorCounts: (State * State) list =
    let isAdjacentLayout
        ({ Generators = g1; Microchips = m1 },
         { Generators = g2; Microchips = m2 }) =

        let rec loop found idx =
            if idx < numVariants then
                match (found, g2[idx] - g1[idx], m2[idx] - m1[idx]) with
                | 0, 0, 0 -> loop 0 (idx + 1)
                | 0, 1, 0
                | 0, 0, 1
                | 1, 0, 0 -> loop 1 (idx + 1)
                | 0, 2, 0
                | 0, 0, 2
                | 0, 1, 1
                | 1, 1, 0
                | 1, 0, 1
                | 2, 0, 0 -> loop 2 (idx + 1)
                | _ -> false
            else
                true

        loop 0 0

    layoutsByFloorCounts
    |> Map.toSeq
    |> Seq.collect (fun (counts, layouts) ->
        adjacentFloorCounts
        |> Map.find counts
        |> Map.map (fun _ ->
            Seq.collect (fun adjCounts -> layoutsByFloorCounts[adjCounts])
            >> Seq.toList)
        |> Map.toSeq
        |> Seq.collect (fun (fromFloor, adjLayouts) ->
            adjLayouts |> Seq.map (fun toLayout -> (fromFloor, toLayout)))
        |> Seq.allPairs layouts
        |> Seq.map (fun (fromLayout, (fromFloor, toLayout)) ->
            ((fromLayout, fromFloor), (toLayout, fromFloor + 1)))
        |> Seq.filter (fun ((fromLayout, _), (toLayout, _)) ->
            isAdjacentLayout (fromLayout, toLayout)))
    |> Seq.toList

let allValidMovements =
    allValidUpMovements
    |> Seq.map (fun (from, to_) -> (to_, from))
    |> Seq.append allValidUpMovements
    |> Seq.groupBy fst
    |> Map.ofSeq
    |> Map.map (fun _ -> Seq.map snd >> Seq.toList)

let shortedDistances =
    let visited = new Dictionary<_, _>()

    visited.Add(startingState, 0)

    let rec loop depth priorVisited =
        let toVisitNext =
            priorVisited
            |> Seq.collect (fun v -> allValidMovements[v])
            |> Seq.distinct
            |> Seq.filter (not << visited.ContainsKey)
            |> Seq.toList

        if toVisitNext.IsEmpty then
            visited
        
        else
            toVisitNext
            |> Seq.map (fun vn -> (vn, depth))
            |> Seq.iter (visited.Add)

            loop (depth + 1) toVisitNext

    loop 1 [startingState]

shortedDistances.Values
|> Seq.max;;