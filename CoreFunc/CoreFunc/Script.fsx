#load "Language.fs"
#load "Compile.fs"

open Language
open Compile

let src = "main = S K K 3"
let src2 = "main = letrec x = 43 in S K K x"
let src3 = "pair x y f = f x y ;
fst p = p K ;
snd p = p K1 ;
f x y = letrec
    a = pair x b ;
    b = pair y a
    in
    fst (snd (snd (snd a))) ;
main = f 3 4"
let src4 = "
f x y = letrec
    a = pair x b ;
    b = pair y a
    in
    fst (snd (snd (snd a))) ;
main = f 3 4"
let g () =
    use f = System.IO.File.CreateText("output.txt")
    runProg src3 |> fprintf f "%s"

runProg src3