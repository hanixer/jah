#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#load "Graphs.fs"
#load "CoolAst.fs"
#load "CoolType.fs"
#load "Parser.fs"

open Parser
open FParsec
open System
(*
let t = run (regex "\"[^\n]*\"") "\"\""
run pCoolInt "2147483648"

let ws = spaces
let str_ws s = pstring s >>. ws

let opp = new OperatorPrecedenceParser<float,unit,unit>()
let expr = opp.ExpressionParser
let term = (pfloat .>> ws) <|> between (str_ws "(") (str_ws ")") expr
opp.TermParser <- term

type Assoc = Associativity

opp.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, fun x y -> x + y))
opp.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, fun x y -> x * y))
*)
run expr "1+2*3+5"