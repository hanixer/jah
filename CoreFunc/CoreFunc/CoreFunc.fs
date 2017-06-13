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

[<EntryPoint>]
let main argv =
    Compile.runProg src3
    printfn "%A" argv
    0 // return an integer exit code
