type Eventually<'T> =
    | Done of 'T
    | NotYetDone of (unit -> Eventually<'T>)

module Eventually =
    let rec bind f = function
        | Done x -> NotYetDone (fun () -> f x)
        | NotYetDone g -> NotYetDone (fun () -> bind f (g()))


type MaybeBuilder() =
    member x.Bind(m, f) =
        printfn "Bind"
        match m with
        | Some v -> f v
        | None -> None

    member x.Return(v) = printfn "Return";Some v

    member x.ReturnFrom(m) = printfn "ReturnFrom";m

    member x.Zero() = printfn "Zero";Some ()

    member x.Delay(f) = printfn "Delay";f

    member x.Run(f) = printfn "Run";f()

    member x.Combine(y, z) = Some  [y, z]

    member x.While(pred, body) =
        printfn "While"
        if pred() |> not then
            printfn "While finished"
            Some ()
        else
            printfn "While body"
            x.Bind(body(), fun() ->
                x.While(pred, body))

let maybe = MaybeBuilder()

let m = maybe {
    printfn "I wee go"
    printfn "So next line"
    let! y = Some 1
    let! u = Some 'i'
    let mutable yy = 1
    // let! x = None
    while yy < 10 do
        yy <- yy + 1
        return ()
}