
open System
open System.Security.Cryptography
open System.Text
open FSharpx


let [<Literal>] PuzzleInput = "wtnhxymk"


let generateHashHeader =

    let md5Hasher = MD5.Create ()

    let bytesToHash =
        Array.zeroCreate<byte> (PuzzleInput.Length + 10)

    let reversedDigits =
        Array.zeroCreate<byte> 10

    ignore <| Encoding.ASCII.GetBytes(PuzzleInput, 0, PuzzleInput.Length, bytesToHash, 0)

    let zeroByte =
        Encoding.ASCII.GetBytes("0")
        |> Array.head

    fun nonce -> 
        let mutable nonce' = nonce
        let mutable length = 0

        while nonce' > 0 do
            reversedDigits.[length] <- zeroByte + byte (nonce' % 10)
            nonce' <- nonce' / 10
            length <- length + 1

        for idx = 0 to (length - 1) do
            bytesToHash.[PuzzleInput.Length + length - 1 - idx] <-
                reversedDigits.[idx]

        ignore <| md5Hasher.TransformFinalBlock (bytesToHash, 0, PuzzleInput.Length + length)

        let hash = md5Hasher.Hash in 
            ((uint hash.[0]) <<< 24) + ((uint hash.[1]) <<< 16) + ((uint hash.[2]) <<< 8) + (uint hash.[3])


[<EntryPoint>]
let main _ =
    
    let hashHeaders =
        Seq.initInfinite id
        |> Seq.choose (fun nonce ->
            let hashHeader = generateHashHeader nonce

            if hashHeader &&& 0xFFFFF000u = 0u then
                Some hashHeader
            else
                None)
        |> Seq.map (fun h -> h.ToString("X8").Substring(5, 2))
        |> Seq.map (Seq.toArray)
        |> Seq.map (function | [| h1; h2 |] -> (h1, h2) | _ -> failwith "Unexpected error." )
        // Cache any hash headers as we find them.
        |> Seq.cache

    // We only need the headers for the first 8 successful nonces.
    hashHeaders
    |> Seq.map fst
    |> Seq.take 8 
    |> Seq.toArray
    |> String
    |> String.toLowerInvariant
    |> printfn "Part 1 answer = %s\n"

    let requiredLocs =
        Set.ofSeq { 0..7 }

    hashHeaders
    |> Seq.map (fun (loc, value) -> (int loc - int '0'), value)
    |> Seq.filter (fun (loc, _) -> loc < 8)
    |> Seq.scan (fun (locsFound, headers) (loc, value) ->
        (locsFound |> Set.add loc,
         if headers |> Map.containsKey loc then
            headers
         else
            headers |> Map.add loc value)) (Set.empty, Map.empty)
    // We only want to keep generating nonces until we have found a letter for each required position of the pasword.
    |> Seq.find (fun (locsFound, _) ->
        requiredLocs |> Set.isSuperset locsFound)
    |> snd
    |> Map.toArray
    |> Array.sortBy fst
    |> Array.map snd
    |> String
    |> String.toLowerInvariant
    |> printfn "Part 2 answer = %s" 

    0