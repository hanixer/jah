#load "Language.fs"
#load "Util.fs"
#load "Tim.fs"
#load "Tim1.fs"
#load "Tim2.fs"

open Language
open Tim

let id1 = "id = S K K ;
id1 = id id ;
main = id1 4"
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
main = fac 10"
let gcd = "gcd a b =
    if (a==b)
        a
        if (a<b)
            (gcd b a)
            (gcd b (a-b)) ;
main = gcd 15 2"
let nfib = "nfib n = if (n==0) 1 (1 + nfib (n-1) + nfib (n-2)) ;
main = nfib 4"
let fib = "
fib n = if (n < 2) 1 (fib (n-1) + fib (n-2));
main = fib 4"
let whnf = "main = K 1"
let simpleFunc = "f x = x + 5;
main = f 4"
let doubl = "double x = x + x;
main = double (2 + 2)"
let lotsOfArgs = "f x1 x2 x3 x4 x5 x6 = K (I (I (I 5))) (I (I (I (I 6)))) ;
main = f 1 2 3 4 5 6"
let four = "four = 2 + 2;
five = 5;
main = four * five"
let factorial = "
factorial n = if n 1 (n * factorial (n - 1));
main = factorial 3"
let simpleIf = "
main = if 1 2 3"
let let1 = "
g x y = y;
h x = x;
f x = let y = f 3 
    in g (let z = 4 in h z) y;
main = 1 + 2"
let withLet = "
f x y z = let p = x + y in p + x + y + z;
main = f 1 2 3"
let withoutLet = "
f1 p x y z = p + x + y + z;
f x y z = f1 (x + y) x y z;
main = f 1 2 3"


let g src =
    use f = System.IO.File.CreateText("output.txt")
    Tim.fullRun src |> fprintf f "%s"

// g lotsOfArgs

g withLet