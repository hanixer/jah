module CoreFunc

let four = "four = 2 + 2;
main = four * four"

[<EntryPoint>]
let main argv =
    Tim.fullRun four |> ignore
    0 // return an integer exit code
