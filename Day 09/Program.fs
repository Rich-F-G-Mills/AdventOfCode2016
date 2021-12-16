
open System
open System.IO


type Data =
    | Marker of HeaderLength: int64 * Times: int64 * Content: Data list
    | Literal of Text: String


// Will recursively expand markers to determine the uncompressed length down to a specified depth.
let rec elementLength recurseToDepth depth =
    function
    | Marker (headerLen, times, content) ->
        let totalChildLength =
            int64 (content |> List.sumBy (elementLength recurseToDepth (depth + 1)))

        if depth < recurseToDepth then
            times * totalChildLength
        else
            headerLen + totalChildLength

    | Literal text ->
        int64 text.Length
            

// Split the list at the first instance the predicate fails.
let partitionWhile pred list =
    match list |> List.tryFindIndex (not << pred) with
    | Some index ->
        list |> List.splitAt index
    | None ->
        list, List.empty
    

let (|LooksLikeNumber|_|) =
    partitionWhile Char.IsDigit
    >> function
        // Make sure that we have at least a single character!
        | _::_ as digits, rest ->
            Some (String.Concat digits, rest)
        | _ -> None     


let rec parseCompressedText (accrued: Data list): char list -> Data list =
    function
    | [] -> accrued |> List.rev
    | IsMarker (element, rest)
    | IsLiteral (element, rest) ->
        parseCompressedText (element :: accrued) rest

    | rest ->
        rest
        |> List.take 10
        |> String.Concat
        |> sprintf "Unable to parse: '%s...'"
        |> failwith

and (|IsMarker|_|) =
    function
    | '('::(LooksLikeNumber (len, 'x'::(LooksLikeNumber (times, ')'::rest)))) ->
        let markerContent, rest' =
            rest |> List.splitAt (int len)

        let parsedMarkerContent =
            parseCompressedText [] markerContent
            
        let newData =
            Marker (int64 (len.Length + times.Length + 3), int64 times, parsedMarkerContent)

        Some <| (newData, rest')
    | _ -> None

and (|IsLiteral|_|) =
    partitionWhile Char.IsUpper
    >> function
        | [], _ -> None
        | text, rest ->
            Some (Literal <| String.Concat text, rest) 
        

[<EntryPoint>]
let main _ =
    let parsedText =
        File.ReadAllText "Inputs.txt"
        |> List.ofSeq
        |> parseCompressedText []

    parsedText
    |> List.sumBy (elementLength 1 0)
    |> printfn "Part 1 answer = %i\n"

    parsedText
    |> List.sumBy (elementLength Int32.MaxValue 0)
    |> printfn "Part 2 answer = %i"

    0