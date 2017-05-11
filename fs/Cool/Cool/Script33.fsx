#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#r @"bin\Debug\Cool.dll"
#load "CoolAst.fs"

open System.IO
open FParsec
open CoolAst
open CoolAst.Deserialize


let primitiveTypes = ["Int";"String";"Bool"]
let completeWithStandartTypes parents =
    parents
    |> List.map (fun (c, p) -> c, defaultArg p "Object")
    |> List.append
    <| List.map (fun t -> t, "Object") primitiveTypes
completeWithStandartTypes ["a",Some "b"; "c", None]