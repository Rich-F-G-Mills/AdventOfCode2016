
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx
open FSharpx.Text


type EncryptedName =
    { Content: string
      SectorId: int
      CheckSum: string }


[<EntryPoint>]
let main _ =

    let encryptedNames =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (function
            | Regex.Match RegexOptions.None @"^([a-z-]+)-(\d+)\[([a-z]+)\]$" 
                { GroupValues = [ content; sectorId; checkSum ] } ->
                    { Content = content; SectorId = int sectorId; CheckSum = checkSum }
            | _ ->
                failwith "Invalid encrypted name.")

    let validEncryptedNames =
        encryptedNames
        |> Array.filter (fun { Content = content; CheckSum = checkSum } ->
            content
            |> String.replace' "-" String.Empty
            |> Array.ofSeq
            |> Array.countBy id
            |> Array.map (fun (chr, count) -> (-count, chr))
            |> Array.sort
            |> Array.map snd
            |> Array.take 5
            |> String
            |> String.equals StringComparison.InvariantCulture checkSum)

    validEncryptedNames
    |> Array.sumBy (fun { SectorId = sectorId } -> sectorId)
    |> printfn "Part 1 answer = %i\n"


    let decryptedNames =
        validEncryptedNames
        |> Array.map (fun { Content = content; SectorId = sectorId } ->
            content
            |> Array.ofSeq
            |> Array.map (function
                | '-' -> '-'
                | chr when chr >= 'a' && chr <= 'z' ->
                    'a' + char (((int chr) - (int 'a') + sectorId) % 26)
                | _ -> failwith "Unexpected character.")
            |> String, sectorId)

    decryptedNames
    |> Array.find (fst >> String.startsWith "northpole")
    |> snd
    |> printfn "Part 2 answer = %i"

    0