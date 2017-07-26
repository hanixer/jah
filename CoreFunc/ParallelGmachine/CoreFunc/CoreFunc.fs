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
let nfib = "nfib n = if (n==0) 1 (1 + nfib (n-1) + nfib (n-2)) ;
main = nfib 4"
let whnf = "main = K 1"


[<EntryPoint>]
let main argv =
    Gm.runProg "main = if (2 > 3 | 3 == 4) 3 4"
    |> printfn "%A"
    0 // return an integer exit code
