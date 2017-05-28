#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#load "Graphs.fs"
#load "CoolAst.fs"
#load "CoolType.fs"
#load "Parser.fs"

open Parser
open FParsec
open System
open System.IO
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

let doit s =
    use file = File.CreateText("testoutput.txt")
    match run cool s with
    | Success (r, _, _) ->
        fprintf file "%A" r
    | Failure (ss, _, _) -> printfn "%A" ss
let doite s =
    use file = File.CreateText("testoutput.txt")
    match run expr s with
    | Success (r, _, _) ->
        fprintf file "%A" r
    | Failure (ss, _, _) -> printfn "%A" ss

doite "{avar <- (new A).method1(avar.value()) \t\t\t\n;1;}"
doit (File.ReadAllText(@"tests\arith.cl"))