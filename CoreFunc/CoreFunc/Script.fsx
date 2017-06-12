#load "Language.fs"
#load "Compile.fs"

open Language
open Compile

let s = "
once upon
a time || there was 
a scooter 23423
but"

let toks = 
    [
        "Pack"
        "{"
        "1"
        ","
        "2"
        "}"
        "1"
        ","
        "2"
        ","
        "3"
        "a"
        ","
        "a"
        "werwerwererwerwea"
        "a"
        "b"
        "231"
        "we"
    ] //|> List.map List.ofSeq

// pZeroOrMore pVar toks
let input = "
f x y = case x of
<1> -> case y of
<1> -> 1;
<2> -> 2 "
let inp2 = "
f x = x;
x e = e e"
inp2 |> List.ofSeq |> clex |> List.map (List.toArray >> string) |> pExpr
parse inp2