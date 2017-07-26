#load "Language.fs"
#load "Util.fs"
#load "Tim.fs"
// #load "Tim1.fs"
// #load "Tim2.fs"
// #load "Tim_4_5_5.fs"
// #load "Tim_4_5_6.fs"

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
let simpleLetrec = "
f x = 
    letrec p = if (x < 1) 1 q;
           q = if (x < 1) p 2
    in p+q;
main = f 1"
let issueLetrec = "
f x = letrec a = b;
             b = x
      in a;
main = f 1"
let optiLet = "
f x y = (let a=1;b=2;c=3 in b+x) + (let f=2;g=4;h=5;j=6 in h+y);
main = f 10 20"
let anotherLetrec = "
g x y = y;
f x = letrec y = 1
      in
      g y y;
main = f 1"
let anotherLet = "
g x y = y;
f x = let y = 1
      in
      g y y;
main = f 1"
let indChain = "
g x = x + 1;
f x = g x;
h x = f x;
main = h (1 + 2 + 3)"
let partAppli = "
add a b = a+b;
twicer f x = f (f x);
g x = add (x*x);
main = twicer (g 3) 4"
let pairs = "
pair x y f = f x y;
first p = p K;
second p = p K1;
main = let w = if (4 < 2*3) (pair 2 3) (pair 3 2)
in (first w) * (second w);
maine = let w = pair 2 3
in (first w) * (second w)"
let threeArgs = "
f x y z = x;
g = f (2 + 3) (3 + 4) (4 + 5);
g2 = f (2 + 3) (3 + 4) 4;
g3 = f (2 + 3) 5 4;
g4 = f 1 2 3;
main = g2 "
let threeArgs2 = "
f x y z = x;
g = if (2 < 3) (3 < 4) (4 < 5);
g2 = if (2 < 3) (3 < 4) 4;
g3 = if (2 < g) 5 4;
g4 = if 1 2 3;
main = g2 "
let simpleIf = "main = let x = (4 + 4) in if (2 < x) 2 3"
let lengt = "
length xs = 
    case xs of
      <1> -> 0;
      <2> p ps -> p + (length ps);
main = length (cons 1 (cons 2 nil))"
let append = "
length xs = 
    case xs of
      <1> -> 0;
      <2> p ps -> 1 + (length ps);
append xs ys = 
    case xs of
        <1> -> ys;
        <2> p ps -> 
            cons p (append ps ys);
main = 
    let xs = append (cons 1 nil) (cons 2 nil)
    in
    length xs + length xs"
let append2 = "
length xs = 
    case xs of
      <1> -> 0;
      <2> p ps -> 1 + (length ps);
append xs ys = 
    case xs of
        <1> -> ys;
        <2> p ps -> 
            cons p (append ps ys);
main = 
    let xs = append (cons 1 nil) (cons 2 nil)
    in
    let l = length xs
    in
    l + l"
let between = "
between n m = if (n<m) (cons n (between (n+1) m)) nil ;
main = between 1 4"
let cafTesting = "
x = 5 + 5 + 3;
y = x + 6;
main = cons (x + x) nil"

let g src =
    use f = System.IO.File.CreateText("output.txt")
    Tim.fullRun src |> fprintf f "%s"

// g lotsOfArgs

g cafTesting