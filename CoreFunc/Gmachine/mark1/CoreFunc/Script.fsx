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
let src7 = "main = Pack{2,2} 1 2"
let src8 = "main = (2 + 3) + (3 * 3)"
let src9 = "fac n = if (n == 0) 1 (n * fac (n-1)) ;
main = fac 1"
let src10 = "f x = x + 10;
g x = 8 * f (x - 1);
main = g 3"

let g src =
    use f = System.IO.File.CreateText("output.txt")
    runProg src |> fprintf f "%s"

g src