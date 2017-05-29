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

let doit s =
    use file = File.CreateText("testoutput.txt")
    match Parser.parse (File.ReadAllText(s)) with
    | Some ast ->
        fprintf file "%A" ast
        CoolType.analyze ast
        |> CoolType.Result.map (fun (semInfo:CoolType.SemanticInfo) ->
            match semInfo.AnnotatedAst with 
            | Ast cs ->
                cs
                |> List.pick (fun (Class ((_, n), _, fs)) -> 
                    if n = "Main" then
                        fs
                        |> List.pick (function
                            | Method ((_, "main"),_,_,body) ->
                                Interpreter.evaluate 
                                    semInfo 
                                    (ObjectVal ("Main", Map.empty)) 
                                    { NewLoc = 0; Dict = System.Collections.Generic.Dictionary<Loc, Value>() }
                                    Map.empty
                                    body
                                |> Some
                            | _ -> None)
                        |> Some
                    else None))
    | None -> 
        printfn "NONONONONON"
        CoolType.Result.Failure []
    // | None -> printfn "Parser failed"

// printfn "%s - current dir" System.Environment.CurrentDirectory
let s=System.IO.File.ReadAllText("tests\\arith.cl") 
doit "tests\\1.cl"