#load "Language.fs"
#load "Util.fs"
#load "Compile.fs"

open Language
open Compile

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
let src4 = "main = letrec f = f x in f"

let src5 = "main = 2 * 3 * 342"
let src = "id = S K K ;
main = twice twice twice id 3"

let g src =
    use f = System.IO.File.CreateText("output.txt")
    runProg src |> fprintf f "%s"

g src