
open System


let [<Literal>] NumElves = 3004953


[<EntryPoint>]
let main _ =

    let elves =
        Array.create NumElves 1

    let rec iterateElves pred =
        match elves |> Array.tryFindIndex pred with
        | Some idx -> idx + 1
        | None -> 
            for idx = 0 to (NumElves - 1) do
                if elves.[idx] > 0 then
                    let mutable offset = 1

                    while elves.[(idx + offset) % NumElves] = 0 do
                        offset <- offset + 1

                    elves.[idx] <- elves.[idx] + elves.[(idx + offset) % NumElves]
                    elves.[(idx + offset) % NumElves] <- 0

            iterateElves pred

    iterateElves ((=) NumElves)
    |> printfn "Part 1 answer = %i\n"
    
    0