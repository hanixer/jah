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

let isCyclesPresent nodes = 
    let dic = Map.ofList nodes
    let rec check c curr =
        
    nodes 
    |> List.exists (fun n -> Seq.from

let parents classes =
    let handle c = className c, parentName c
    classes
    |> List.map handle
// let methodMap (Ast classes) = 
//     let handleClass c = 
//         match c with
//         | Class (name, parent, 
//     1