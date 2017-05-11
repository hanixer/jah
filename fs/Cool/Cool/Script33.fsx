#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#r @"bin\Debug\Cool.dll"
//#load "CoolAst.fs"

open System.IO
open FParsec
open CoolAst
open CoolAst.Deserialize

run pExpr "1\nnew\n3\nbbb\n"

let x = Choice1Of2 1
Seq.append (seq {yield 1}) (seq {1..5})