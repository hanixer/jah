#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#r @"bin\Debug\Cool.dll"
//#load "CoolAst.fs"

open System.IO
open FParsec
open CoolAst
open CoolAst.Deserialize

let nodes = [(1,2);(2,3);(4,3);5,4;8,7;7,6;6,8;6,9]
let dic = Map.ofList nodes
let isCyclesPresent nodes = 
    let dic = Map.ofList nodes
    let updateRoots oldRoot newRoot roots =
        roots
        |> Map.map (fun k v -> if v = oldRoot then newRoot else v)
        |> Map.add oldRoot newRoot 

    let rec f parents roots = 
        // printfn "%A" roots
        match parents with
        | [] -> false
        | (c, p)::tail ->
            if c == p then
                true
            match Map.tryFind p roots with
            | Some x when x = c || c = p -> true
            | _ -> f tail (updateRoots c p roots)

    f nodes Map.empty
isCyclesPresent [2,2;3,4]
isCyclesPresent [1,2;2,3;3,4;4,2]
isCyclesPresent [1,2;2,3;3,4;5,4;7,4;4,1]