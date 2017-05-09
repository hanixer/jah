// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open CoolAst
open CoolAst.Deserialize
open FParsec

[<EntryPoint>]
let main argv = 
    initIt 54
    run pExpr "JIJIJIJ"
    printfn "%A" argv
    0 // return an integer exit code
