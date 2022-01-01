
open System


let strToBoolean =
    Array.ofSeq
    >> Array.map ((=) '1')

let booleanToStr =
    Array.map (function | false -> "0" | true -> "1")
    >> String.concat String.Empty


let PuzzleInput =
    strToBoolean "11011110011011101"


let iterateData (a: bool []) =
    let b =
        a
        |> Array.copy
        |> Array.rev
        |> Array.map not

    Array.concat [| a; [| false |]; b |]

let calcCheckSum (data: bool []) =
    let rec inner data =
        let checkSum =
            data
            |> Array.chunkBySize 2
            |> Array.map (function [| a; b |] -> a = b | _ -> failwith "Unknown error.")

        if checkSum.Length % 2 = 0 then
            inner checkSum
        else
            checkSum

    inner data


[<EntryPoint>]
let main _ =

    let dataSeq =
        Seq.unfold (fun state ->
            let newState = iterateData state

            Some (newState, newState)) PuzzleInput

    let getCheckSumForLen len =
        dataSeq
        |> Seq.skipWhile (fun state -> state.Length < len)
        |> Seq.head
        |> Array.take len
        |> calcCheckSum
        |> booleanToStr

    getCheckSumForLen 272
    |> printfn "Part 1 answer = %s\n"

    getCheckSumForLen 35_651_584
    |> printfn "Part 2 answer = %s"

    0