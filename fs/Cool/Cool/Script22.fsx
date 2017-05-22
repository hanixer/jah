#load "CoolType.fs"
open CoolType
let s = Success 5
let f = fun () -> Success 5
let g = fun () -> Failure [5]
result {
    do! Failure [5]
    do! g()
    do! Failure [5]
    let! _ = Success 5
    return 555
}
result.Bind(Failure [5], fun () -> Success 5)
result.Bind(Success 5, fun _ -> Success 5)