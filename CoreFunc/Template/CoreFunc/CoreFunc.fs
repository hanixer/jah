module CoreFunc

[<EntryPoint>]
let main argv =
    printfn "%A" (List.distinct [1;2;1;2;3])
    0 // return an integer exit code
