module CoreFunc


let src3 = "pair x y f = f x y ;
fst p = p K ;
snd p = p K1 ;
f x y = letrec
    a = pair x b ;
    b = pair y a
    in
    fst (snd (snd (snd a))) ;
main = f 3 4"

let src = "main = 3 + 4"
let src7 = "main = Pack{2,2} 1 2"
let src6 = "f x = Pack{2,0};
main = if Pack{2,0} 1 2"
let src9 = "fac n = if (n == 0) 1 (n * fac (n-1)) ;
main = fac 1"
let src10 = "f x = x;
main = f (if True (f 1) (f 2))"
let src11 = "f x = x + x;
main = f (if True (2 + 3) (3 + 3))"

[<EntryPoint>]
let main argv =
    Compile.runProg src9
    |> printfn "%s"
    0 // return an integer exit code
