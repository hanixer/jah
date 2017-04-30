module Parfs

type Result<'T> =
    | Success of 'T
    | Failure of string

type Parser<'T> = string -> Result<'T * string>

let parseChar inp =
    if System.String.IsNullOrEmpty(inp) then
        Failure "String is empty"
    else
        Success (inp.[0], inp.[1..])

let parsePred pred inp = 
    match parseChar inp with
    | Success (c, rest:string) -> 
        if pred c then
            Success (c, rest)
        else
            Failure "Predicate didn't matched"
    | Failure f -> Failure f

let parseLower = parsePred (fun (c:char) -> System.Char.IsLower(c))

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
