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
    | MethodInheritedFormalsLengthDiffer of string * Id
    | MethodInheritedFormalTypeDiffer of (*class*)Id * (*method*)Id * (*current type*)Id * (*expected type*)Id

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

// TODO: add check for inheriting from primitive types
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

let ast2inheritanceGraph ast =
        ast
        |> inheritanceMap
        |> mapRes Graphs.edges2adjacency

type MethodSignature = Id * Formal list * Id

let str2id s = (0, s)

let class2methodsSignatures c =
    c
    |> methodsOf 
    |> List.map (fun (a,b,c,_) -> a,b,c)

let unwrapClass (Class (a,b,c)) = a,b,c

let tryFindClass c (Ast cs) =
    cs
    |> List.tryFind (fun (Class ((_, c2), _, _)) -> c = c2)

let getClassMethods c ((Ast cs) as ast) : MethodSignature list =
    let methods () = 
        match tryFindClass c ast with
        | Some cl ->
            class2methodsSignatures cl
        | None -> 
            []
    match c with
    | "Object" ->
        [   str2id "abort", [], str2id "Object"
            str2id "type_name", [], str2id "String"
            str2id "copy", [], str2id "SELF_TYPE" ]
    | "IO" -> 
        [   str2id "out_string", [str2id "x", str2id "String"], str2id "SELF_TYPE"
            str2id "out_int", [str2id "x", str2id "Int"], str2id "SELF_TYPE"
            str2id "in_string", [], str2id "String"
            str2id "in_int", [], str2id "Int" ]
    | _ -> 
        methods ()

let getInheritedMethods c ast inhMap : MethodSignature list =
    let merge ms1 ms2 =
        ms1
        |> List.filter (fun ((_, m1), _, _) ->
            ms2
            |> List.tryFind (fun ((_, m2), _, _) ->
                m1 = m2)
            |> Option.isNone)
        |> List.append ms2
    let rec go c =
        match Map.tryFind c inhMap with
        | None -> []
        | Some p ->
            go p
            |> merge
            <| getClassMethods p ast

    go c
// Sort classes
// For each class
//  get inherited methods
//  get current methods
//  group methods with the same name
//  check for:
//   number of args
//   types of args
//   return type

let groupMethodsByName ms1 ms2 =
    ms1
    |> List.fold (fun res (((_, n1), _, _) as m1) ->
        match List.tryFind (fun ((_, n2), _, _) -> n1 = n2) ms2 with
        | None -> res
        | Some m2 -> (m1, m2) :: res) []

let formal2type = snd >> snd

let getInheritedMethodsErrors c ast inhMap : TypeError list =
    let inhMs = getInheritedMethods c ast inhMap
    let myMs = getClassMethods c ast
    let merged = groupMethodsByName myMs inhMs
    let formalLengthError (m1, formals1, _) (_, formals2, _) =
        if List.length formals1 <> List.length formals2 then
            MethodInheritedFormalsLengthDiffer (c, m1) |> Some
        else None
    let formalTypeError (m1, formals1, _) (_, formals2, _) =
        List.zip formals1 formals2
        |> List.tryFind (fun (f1, f3) -> formal2type f1 <> formal2type f2)
        |> Option.map (fun (
    []
    

let validateRedefinedMethods ast =
    let inhMap = ast |> classesFromAst |> completeWithStandartTypes |> Map.ofList
    ast 
    |> ast2inheritanceGraph 
    |> mapRes Graphs.toposort
    