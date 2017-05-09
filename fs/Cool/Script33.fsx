#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#r @"bin\Debug\Cool.dll"
//#load "CoolAst.fs"

open FParsec
open CoolAst
open CoolAst.Deserialize
open System.IO

// let p = pAst |>> ignore

CoolAst.Deserialize.initIt()
run pExpr "1"

// let jijojioj = IdentifierOptions

// run CoolAst.Deserialize.pAst "fer"
// Define your library scripting code here
