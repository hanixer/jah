#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#load "Graphs.fs"
#load "CoolAst.fs"
#load "CoolType.fs"
#load "Parser.fs"
#load "Interpreter.fs"

open CoolAst
open Parser
open FParsec
open System
open System.IO
open Interpreter

let dointerp semInfo =
    try 
        match Interpreter.execute semInfo with
        | CoolType.Success x -> 
            printfn "Executed successfully.\n%A" x
        | CoolType.Failure errs ->
            printfn "Execution failed.\n%A" errs
    with
    | :? System.StackOverflowException ->
        printfn "Stack overflow in execute"


let doit s =
    try 
        use file = File.CreateText("testoutput.txt")
        match Parser.parse (File.ReadAllText(s)) with
        | Some ast ->
            // fprintf file "%A" ast
            CoolType.analyze ast
            |> CoolType.Result.map dointerp
        | None -> 
            printfn "NONONONONON"
            CoolType.Result.Failure []
    with
    | :? System.StackOverflowException ->
        printfn "Stack overflow in doit"
        CoolType.Result.Failure []
    // | None -> printfn "Parser failed"

// printfn "%s - current dir" System.Environment.CurrentDirectory
let s=System.IO.File.ReadAllText("tests\\arith.cl") 
doit "tests\\1.cl"