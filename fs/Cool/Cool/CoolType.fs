module CoolType

open CoolAst

type Result<'a> = 
    | Success of 'a
    | Failure of string list

type Signature = string list
type MethodEnv = Map<string * string, Signature>

type ObjectEnv = Map<string, string>

let className (Class (id, _, _)) = snd id
let parentName (Class (_, p, _)) = p |> defaultArg <| (0, "Object") |> snd

let checkNoRedefinition (classes : string list) = false
let checkAllParentsValid (classes : string list) (parents : string list) = true

let isCyclesPresent nodes = 
    let dic = Map.ofList nodes
    let updateRoots oldRoot newRoot roots =
        roots
        |> Map.map (fun k v -> if v = oldRoot then newRoot else v)
        |> Map.add oldRoot newRoot 

    let rec f parents roots = 
        match parents with
        | [] -> false
        | (c, p)::tail ->
            match Map.tryFind p roots with
            | None -> f tail (Map.add c p roots)
            | Some x when x = c -> true
            | Some x -> f tail (updateRoots c p roots)

    f nodes Map.empty
    
let parents classes =
    let handle c = className c, parentName c
    classes
    |> List.map handle
// let methodMap (Ast classes) = 
//     let handleClass c = 
//         match c with
//         | Class (name, parent, 
//     1