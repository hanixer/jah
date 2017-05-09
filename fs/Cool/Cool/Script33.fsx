#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#r @"bin\Debug\Cool.dll"
//#load "CoolAst.fs"

open System.IO
open FParsec
open CoolAst
open CoolAst.Deserialize

run pExpr "1\ninteger\n3\n"
run pExpr "1\nnew\n3\nbbb\n"


