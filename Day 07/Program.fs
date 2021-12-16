
open System.IO
open System.Text.RegularExpressions


type Address =
    { Supernets: string array
      Hypernets: string array }

// A function factory to create an extractor for a given pattern.
let createExtractor pattern =
    let regex =
        new Regex (pattern);

    fun address ->
        regex.Matches (address)
        |> Seq.map (fun m -> m.Value)
        |> Seq.toArray

let extractSupernets =
    createExtractor @"(?<=^|\])[a-z]+(?=$|\[)"

let extractHypernets =
    createExtractor @"(?<=\[)[a-z]+(?=\])"

// There is some duplication here, however not worth creating a palindrom checker for arbitrary lengths.
let hasAnyPalindromesOfLength4: (string array -> bool) =
    Array.collect (Array.ofSeq >> Array.windowed 4)
    >> Array.exists (function
        | [| m1; m2; m3; m4 |] when m1 <> m2 && m2 = m3 && m1 = m4 -> true
        | _ -> false)

let findAllPalindromesOfLength3: (string array -> (char * char * char) array) =
    Array.collect (Array.ofSeq >> Array.windowed 3)
    >> Array.choose (function
        | [| m1; m2; m3 |] when m1 <> m2 && m1 = m3 -> Some (m1, m2, m3)
        | _ -> None)


[<EntryPoint>]
let main _ =

    let addresses =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (fun address ->
            { Supernets = extractSupernets address; Hypernets = extractHypernets address })

    addresses
    |> Array.filter (fun { Supernets = supernets; Hypernets = hypernets } ->
        (hasAnyPalindromesOfLength4 supernets) && (not (hasAnyPalindromesOfLength4 hypernets)))
    |> Array.length
    |> printfn "Part 1 answer = %i\n"

    addresses
    |> Array.filter (fun { Supernets = supernets; Hypernets = hypernets } ->
        let supernetPalindromes =
            findAllPalindromesOfLength3 supernets
            |> Set.ofArray

        let hypernetPalindromes =
            findAllPalindromesOfLength3 hypernets
            // We need to reverse the ordering of the hypernet palindromes
            // in order to make comparable against the supernets above.
            |> Array.map (function
                | (m1, m2, _) -> (m2, m1, m2))
            |> Set.ofArray

        supernetPalindromes
        |> Set.intersect hypernetPalindromes
        |> Set.isEmpty
        |> not)
    |> Array.length
    |> printfn "Part 2 answer = %i"
            
    0