type Eventually<'T> =
    | Done of 'T
    | NotYetDone of (unit -> Eventually<'T>)

module Eventually =
    let rec bind f = function
        | Done x -> NotYetDone (fun () -> f x)
        | NotYetDone g -> NotYetDone (fun () -> bind f (g()))

    let 


Done 1 
|> Eventually.bind (fun x -> x + 1 |> Done)