module CoreFunc

let four = "four = 2 + 2;
main = four * four"

let cafTesting = "
x = 5 + 5 + 3;
y = x + 6;
main = x + x"

[<EntryPoint>]
let main argv =
    Tim.fullRun cafTesting |> ignore
    0 // return an integer exit code
