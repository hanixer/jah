module CoolType

open CoolAst

type Result<'a, 'Error> = 
    | Success of 'a
    | Failure of 'Error list
module Result =
    let bind = 0
type TypeError =
    | ClassRedefined of string
    | ClassesInheritanceCircle of string list
    | MethodRedefined of Id * Id
    | MethodFormalsRedefined of Id * Id * Id

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

let validateRedefinition (classes:list<string*string option>) = 
    let getDuplicated xs =
        xs
        |> List.countBy id
        |> List.filter (snd >> ((<) 1))
        |> List.map fst
    let report redefined =
        if List.isEmpty redefined then
            Success classes
        else
            redefined 
            |> List.map ClassRedefined
            |> Failure
    
    classes
    |> List.map fst
    |> List.append standartTypes
    |> getDuplicated
    |> report

let validateCycles classes =
    let report cycles =
        if List.isEmpty cycles then
            Success classes
        else
            cycles
            |> List.map ClassesInheritanceCircle
            |> Failure

    classes    
    |> List.filter (snd >> Option.isSome)
    |> List.map (fun (c, p) -> c, Option.toObj p)
    |> findCycles
    |> report

let classesFromAst (Ast cs)  : (string * string option) list  =
    let convertToStr (Class (c, pOpt, _)) =
        id2str c, Option.map id2str pOpt
    cs
    |> List.map convertToStr

let completeWithStandartTypes parentsOpt =
    parentsOpt
    |> List.map (fun (c, p) -> c, defaultArg p "Object")
    |> List.append
    <| List.map (fun t -> t, "Object") ("IO" :: primitiveTypes)

let bind f = function
    | Success x -> f x
    | Failure errs -> Failure errs

let mapRes f = function
    | Success x -> f x |> Success
    | Failure errs -> Failure errs

let ret x = function
    | Success _ -> Success x
    | Failure errs -> Failure errs

let inheritanceMap ast =    
    ast
    |> classesFromAst
    |> validateRedefinition
    |> bind validateCycles
    |> mapRes completeWithStandartTypes

let isMethod = function
    | Method _ -> true
    | _ -> false

let validateMethodRedefinition (Ast cs) = 
    let getDuplicated (Class (c, _, fs)) =
        fs
        |> List.choose (function
            | Method (m, _, _, _) -> Some (c, m)
            | _ -> None)
        |> List.fold (fun dic (c, m) ->
            let key = (c, snd m)
            let value = (c, m)
            if Map.containsKey key dic then
                Map.add key (value :: dic.[key]) dic
            else
                Map.add key [value] dic)
            Map.empty
        |> Map.toList
        |> List.map 
            (snd >> List.sort >> List.tail)

    let report duplicated =
        if List.isEmpty duplicated then
            Success (Ast cs)
        else
            duplicated
            |> List.map MethodRedefined
            |> Failure

    cs
    |> List.collect getDuplicated
    |> List.concat
    |> report

let formalName = fst >> snd

let methodsOf (Class (c, _, fs)) =    
    List.choose (function
        | Method (m, m2, m3, m4) -> Some (m, m2, m3, m4)
        | _ -> None) fs

let getDuplicatedFormals(name:Id, formals:Formal list, _, _) =
    let isDuplicated x y = 
        y <> x && formalName y = formalName x
    let rec go : (Id * Id) list -> (Id * Id) list = function
        | x :: xs -> 
            match List.tryFind (isDuplicated x) formals with
            | Some y -> [name, fst y]
            | None -> go xs
        | [] -> []
    match go formals with
        | x :: _ -> Some x
        | [] -> None


let validateMethodFormalsRedefinition (Ast cs) =
    let validateClass ((Class (name, _, features)) as c) =
        c
        |> methodsOf
        |> List.choose getDuplicatedFormals
        |> List.map (fun (m, p) -> name, m, p)
    let report duplicated =
        if List.isEmpty duplicated then
            Success (Ast cs)
        else
            duplicated
            |> List.map MethodFormalsRedefined
            |> Failure
    cs
    |> List.collect validateClass
    |> report

let toposort (edges:Map<'T, 'T option>) : 'T list =
    let head = edges |> Map.toList |> List.head
    if Map.isEmpty edges then []
    else
        []