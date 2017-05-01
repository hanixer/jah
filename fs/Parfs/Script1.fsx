#r @"..\packages\FParsec\lib\net40-client\FParsecCS.dll"  
#r @"..\packages\FParsec\lib\net40-client\FParsec.dll"  

open FParsec

let test p str = 
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(f, _, _) -> printfn "Failure: %s" f