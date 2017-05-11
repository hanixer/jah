module CoolType

open CoolAst

type Result<'a> = 
    | Success of 'a
    | Failure of string list
module Result =
    let bind = 0

type Signature = string list
type MethodEnv = Map<string * string, Signature>

type ObjectEnv = Map<string, string>

let id2str = snd

let className (Class (id, _, _)) = snd id
let parentName (Class (_, p, _)) = p |> defaultArg <| (0, "Object") |> snd

let checkNoRedefinition (classes : string list) = false
let checkAllParentsValid (classes : string list) (parents : string list) = true

    
let parents classes =
    let handle c = className c, parentName c
    classes
    |> List.map handle

let updateRoots clas parent root roots =
    roots
    |> Map.add clas (parent, root)
    |> Map.map (fun c (p, r) -> 
        if r = clas then (p, root) 
        else (p, r))

let addCycle (roots:Map<'a,'a * 'a>) root cycles = 
    let rec go curr =
        let next = roots.[curr] |> fst
        if  next = root then 
            [curr]
        else
            curr :: go next
    go root :: cycles

let findCycles parents = 
    let iter (roots, cycles) (c, p) =
        if c = p then
            // we have a cycle
            let roots' = Map.add c (c,c) roots
            let cycles' = addCycle roots' c cycles
            roots', cycles'
        else 
            match Map.tryFind p roots with
            | Some (pp, rp) -> 
                if c = rp then
                    let roots' = Map.add c (p, c) roots
                    let cycles' = addCycle roots' c cycles
                    roots', cycles'
                else
                    let roots' = 
                        roots |> updateRoots c p rp
                    roots', cycles
            | None ->
                let roots' = 
                    roots |> updateRoots c p p
                roots', cycles

    parents
    |> List.fold iter (Map.empty, List.empty)
    |> snd

let primitiveTypes = ["Int";"String";"Bool"]
let standartTypes = "Object" :: "IO" :: primitiveTypes

let validateRedefinition classes = 
    let classes = List.append classes standartTypes
    (classes |> List.length)  = 
        (classes |> List.distinct |> List.length)

let validateCycles parents =
    let cycles = findCycles parents
    if cycles.Length > 0 then
        Failure ["There are cycles. TODO: add classes here"]
    else
        Success true

let classParent (Ast cs)  : list<string * string option>  =
    cs
    |> List.map (fun (Class (c, pOpt, _)) -> 
        id2str c, Option.map id2str pOpt)

let completeWithStandartTypes parentsOpt =
    parentsOpt
    |> List.map (fun (c, p) -> c, defaultArg p "Object")
    |> List.append
    <| List.map (fun t -> t, "Object") ("IO" :: primitiveTypes)

let inheritanceMap ast : Result<'a> =    
    let parentsOpt = classParent ast
    let redef = parentsOpt |> List.map fst |> validateRedefinition
    let cycles = 
        parentsOpt 
        |> List.filter (snd >> Option.isSome)
        |> List.map (fun (c, p) -> c, Option.toObj p)
        |> validateCycles
    let completeParents = completeWithStandartTypes parentsOpt
    Failure [""]