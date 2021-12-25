
open System
open System.Security.Cryptography
open System.Text


let [<Literal>] PuzzleInput = "yjdafjpo"


let generateHasher (salt: string) numRepeats =

    let md5Hasher = MD5.Create ()

    let bytesToHash =
        Array.zeroCreate<byte> (PuzzleInput.Length + 10)

    let reversedDigits =
        Array.zeroCreate<byte> 10

    ignore <| Encoding.ASCII.GetBytes(salt, 0, salt.Length, bytesToHash, 0)

    let zeroByte =
        Encoding.ASCII.GetBytes("0")
        |> Array.head

    let byteHexMap =
        { 0uy..255uy }
        |> Seq.map (fun c -> (c, c |> sprintf "%02x" |> Encoding.ASCII.GetBytes))
        |> Map.ofSeq

    let hexedHash =
        Array.zeroCreate 32

    let hexifyHash () =
        for idx = 0 to 15 do
             do byteHexMap.[md5Hasher.Hash.[idx]].CopyTo(hexedHash, idx * 2)

    fun nonce -> 
        let mutable nonce' = nonce
        let mutable length = 0

        if nonce' > 0 then
            while nonce' > 0 do
                reversedDigits.[length] <- zeroByte + byte (nonce' % 10)
                nonce' <- nonce' / 10
                length <- length + 1

            for idx = 0 to (length - 1) do
                bytesToHash.[salt.Length + length - 1 - idx] <-
                    reversedDigits.[idx]

        else
            bytesToHash.[salt.Length] <- zeroByte
            length <- 1

        ignore <| md5Hasher.TransformFinalBlock (bytesToHash, 0, salt.Length + length)

        for x = 0 to (numRepeats - 1) do
            do hexifyHash ()

            ignore <| md5Hasher.TransformFinalBlock (hexedHash, 0, 32)

        Convert.ToHexString md5Hasher.Hash

// Checks if a string has at least n repeated characters.
let hasRepeats n =
    let rec checkOffsetFromIdx (hash: string) toFind idx offset =
        if hash.[idx+offset] = toFind then
            if offset = n - 1 then Some toFind
            else checkOffsetFromIdx hash toFind idx (offset + 1)
        else checkFromIdx hash (idx + 1)
    
    and checkFromIdx (hash: string) idx =
        if idx > hash.Length - n then
            None
        else
            checkOffsetFromIdx hash hash.[idx] idx 1
            
    fun (hash: string) ->
        checkFromIdx hash 0

// Find all hashes which contain at least 3 repeated (adjacent) characters.
let getHashesWithTriples hasher =
    Seq.initInfinite hasher
    |> Seq.map (fun hash -> (hash, hasRepeats 3 hash))
    |> Seq.indexed
    |> Seq.map (fun (idx, (hash, someLetter)) -> (idx, hash, someLetter))
    |> Seq.filter (function | _, _, Some _ -> true | _ -> false)
    |> Seq.map (fun (idx, hash, someLetter) -> (idx, hash, someLetter.Value))
    // We make sure this is cached as we will be iterating over this sequence multiple times.
    |> Seq.cache

// Of those with the repeated letters, we need to see if there exists a hash with 5 repeats within the next 1000 hashes.
let findValidOneTimeKeys (hashSeq: seq<int * string * char>) =
    let charRepeats =
        { '0'..'9' }
        |> Seq.append { 'A'..'F' }
        |> Seq.map (fun c -> c, String(c, 5))
        |> Map.ofSeq

    hashSeq
    |> Seq.filter (fun (idx, _, letter) ->
        hashSeq
        |> Seq.skipWhile (fun (idx', _, _) -> idx' <= idx)
        |> Seq.takeWhile (fun (idx', _, _) -> idx' <= idx + 1000)
        |> Seq.exists (fun (_, hash', _) -> hash'.Contains (charRepeats.[letter])))
            

[<EntryPoint>]
let main _ =

    let md5HasherNoRepeats =
        generateHasher PuzzleInput 0  
        
    let md5HasherWithRepeats =
        generateHasher PuzzleInput 2016

    getHashesWithTriples md5HasherNoRepeats
    |> findValidOneTimeKeys
    |> Seq.take 64   
    |> Seq.last
    |> function | idx, _, _ -> idx
    |> printfn "Part 1 answer = %i\n"   

    getHashesWithTriples md5HasherWithRepeats
    |> findValidOneTimeKeys
    |> Seq.take 64   
    |> Seq.last
    |> function | idx, _, _ -> idx
    |> printfn "Part 2 answer = %i"
                       
    0