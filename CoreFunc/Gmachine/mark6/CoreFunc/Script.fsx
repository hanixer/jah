#load "Language.fs"
#load "Util.fs"
#load "Gm.fs"

open Language
open Gm

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

let src5 = "id x = x ;
main = twice twice id 3"
let src = "id = S K K ;
main = twice id 3"
let src6 = "main = 1 + 2 + 3"
let arithm1 = "main = 4*5+(2-5)"
let arithm2 = "inc x = x + 1;
main = twice twice inc 4"
let fac = "fac n = if (n==0) 1 (n * fac (n-1)) ;
main = fac 2"
let gcd = "gcd a b =
    if (a==b)
        a
        if (a<b)
            (gcd b a)
            (gcd b (a-b)) ;
main = gcd 4 2"
let nfib = "nfib n = if (n==0) 1 (1 + nfib (n-1) + nfib (n-2)) ;
main = nfib 4"
let whnf = "main = K 1"

let g src =
    use f = System.IO.File.CreateText("output.txt")
    runProg src |> fprintf f "%s"
parse "main = (2-3)"
g gcd