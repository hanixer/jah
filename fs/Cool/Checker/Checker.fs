module Checker

open System.IO

[<EntryPoint>]
let main argv =
    match argv with
    | [|arg|] ->
        if File.Exists(arg) then
            let text = File.ReadAllText(arg)
            match CoolAst.parse text with
            | Some ast ->
                
                0
            | None ->
                printfn "Parse error"
                1
        else
            printfn "File %s does not exist" arg
            1
    | _ -> 
        printfn "Wrong invocation. Expected exactly one argument"
        -1