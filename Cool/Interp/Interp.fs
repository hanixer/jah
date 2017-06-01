module Interp

open System.IO

let startInterpreter semInfo =
    try
        match Interpreter.execute semInfo with
        | CoolType.Success x ->
            printfn "Executed successfully."
        | CoolType.Failure errs ->
            printfn "Execution failed.\nErrors:\n%A" errs
    with
    | e ->
        printfn "%s" e.StackTrace

[<EntryPoint>]
let main argv =
    try
        match argv with
        | [| filename |] ->
            if File.Exists(filename) then
                match Parser.parse (File.ReadAllText(filename)) with
                | CoolType.Success ast ->
                    match CoolType.analyze ast with
                    | CoolType.Success semInfo ->
                        startInterpreter semInfo
                    | CoolType.Failure errs ->
                        printfn "Semantic analyzer failed.\n%A" errs
                | CoolType.Failure err -> 
                    err
                    |> List.tryHead
                    |> defaultArg
                    <| ""
                    |> printfn "Parsing failed.\n%A"
            else
                printfn "File \"%s\" does not exist" filename
        | _ ->
            printfn "Exactly one Cool file is expected."
            printfn "Usage:"
            printfn "%s [filename.cl]" System.Environment.CommandLine
        0 // return an integer exit code
    with
    | e ->
        printfn "Exception in main catched!!!"
        printfn "%s" e.StackTrace
        0
