#load "Language.fs"

open Language

let s = "
once upon
a time || there was 
a scooter 23423
but"

s |> List.ofSeq |> clex2 0
pLit "a" ["a2";"b";"eee"]
pThen (fun a b -> a) (pLit "a") (pLit "c") ["a";"c"; "b"]
pThen3 (fun a b c -> a |>List.append<| b |>List.append<| c) pVar pVar pVar (["jioj";"weWEJIO";"jJikOJ"] |> List.map List.ofSeq)
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
    ] |> List.map List.ofSeq

// pZeroOrMore pVar toks
let input = "
f x y = case x of
<1> -> case y of
<1> -> 1;
<2> -> 2 "
let inp2 = "
a b"
parseExpr "a * b JEWIORJOEWIJIOREWIJORIEJW *c*e"