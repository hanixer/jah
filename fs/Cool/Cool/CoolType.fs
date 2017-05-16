module CoolType

open CoolAst

type Type =
    | Type of string
    | SelfType of string

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
    | MethodInheritedFormalTypeDiffer of (*class*)string * (*method*)Id * (*formal name*) Id * (*current type*)Id * (*expected type*)Id
    | MethodInheritedReturnTypeDiffer of string * Id * Id * Id
    | AttributeRedefined of string * Id

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

let mergeMethods currMethods baseMethods =
    currMethods
    |> List.filter (fun ((_, m1), _, _) ->
        baseMethods
        |> List.tryFind (fun ((_, m2), _, _) ->
            m1 = m2)
        |> Option.isNone)
    |> List.append baseMethods

let getInheritedMethods c ast inhMap : MethodSignature list =
    let rec go c =
        match Map.tryFind c inhMap with
        | None -> []
        | Some p ->
            go p
            |> mergeMethods
            <| getClassMethods p ast

    go c

let groupMethodsByName ms1 ms2 =
    ms1
    |> List.fold (fun res (((_, n1), _, _) as m1) ->
        match List.tryFind (fun ((_, n2), _, _) -> n1 = n2) ms2 with
        | None -> res
        | Some m2 -> (m1, m2) :: res) []

let formal2type : Formal -> string = snd >> snd

let getInheritedMethodsErrors c ast inhMap : TypeError list =
    let inhMs = getInheritedMethods c ast inhMap
    let myMs = getClassMethods c ast
    let merged = groupMethodsByName myMs inhMs
    let formalLengthError ((m1, formals1, _), (_, formals2, _)) =
        if List.length formals1 <> List.length formals2 then
            MethodInheritedFormalsLengthDiffer (c, m1) |> Some
        else None
    let formalTypeError ((m1, formals1, _), (_, formals2, _)) =
        if List.length formals1 <> List.length formals2 then
            None
        else
            List.zip formals1 formals2
            |> List.tryFind (fun (f1, f2) -> formal2type f1 <> formal2type f2)
            |> Option.map (fun (f1, f2) -> 
                MethodInheritedFormalTypeDiffer (c, m1, fst f1, snd f1, snd f2)) 
    let formalReturnTypeError ((m1, _, rt1), (_, _, rt2)) =
        if snd rt1 <> snd rt2 then
            MethodInheritedReturnTypeDiffer (c, m1, rt1, rt2) 
            |> Some
        else None
    
    merged
    |> List.collect (fun x -> 
        [   formalLengthError x
            formalTypeError x
            formalReturnTypeError x ])
    |> List.choose id

let inheritanceMapUnchecked<'a> : (Ast<'a> -> Map<string,string>)= 
    classesFromAst >> completeWithStandartTypes >> Map.ofList

let validateRedefinedMethods ast =
    let inhMap = ast |> inheritanceMapUnchecked
    ast 
    |> ast2inheritanceGraph 
    |> mapRes Graphs.toposort
    |> mapRes (List.collect (fun c -> getInheritedMethodsErrors c ast inhMap))
    |> bind (fun errs -> 
        if List.isEmpty errs then
            Success ast
        else
            Failure errs)

let class2attributes (Class (_, _, features)) =
    features
    |> List.choose (function
        | Attribute (a,b,c) -> Some (a,b,c)
        | _ -> None)

let getInheritedAttributes c ast inhMap =
    let rec go c =
        match Map.tryFind c inhMap with
        | None -> []
        | Some p -> 
            let selfAttrs = 
                tryFindClass p ast
                |> Option.map class2attributes
                |> defaultArg
                <| []
            go p
            |> List.append  
            <| selfAttrs
    go c

let getInheritedAttributesErrors c ast inhMap = 
    1

let attributeName ((_, a), _, _) = a

let getRedefinedAttribute c ast inhMap =
    let isRedefined x y =
        x <> y && attributeName x = attributeName y
    let baseAttrs = getInheritedAttributes c ast inhMap
    let selfAttrs =
        tryFindClass c ast
        |> Option.map class2attributes
        |> defaultArg
        <| []
    let attrs = List.append baseAttrs selfAttrs
    attrs
    |> List.tryPick (fun x ->
        List.tryFind (isRedefined x) attrs)

let validateRedefinedAttributes ast = 
    let inhMap = ast |> inheritanceMapUnchecked
    let g = Graphs.edges2adjacency (Map.toList inhMap)
    let sorted = Graphs.toposort g
    let errors =
        sorted
        |> List.choose (fun c -> 
            getRedefinedAttribute c ast inhMap
            |> Option.map (fun (a, _, _) ->
                AttributeRedefined (c, a)))
    
    if List.isEmpty errors then
        Success ast
    else Failure errors

type MethodType = string list * string // t1 .. tn - argument types, return type

let getAllClassMethods c ast inhMap = 
    getClassMethods c ast
    |> mergeMethods
    <|  getInheritedMethods c ast inhMap

let class2name (Class ((_, cname), _, _)) = cname

let methodSignature2methodType ((_, m), formals, rt) = 
    formals |> List.map formal2type, snd rt

let ast2methodEnvironment (Ast cs as ast) =
    let inhMap = ast |> inheritanceMapUnchecked
    let extractFromClass = fun c ->
        Map.empty
        |> List.fold (fun meth2type ((_, m), formals, rt) ->
            meth2type
            |> Map.add m  
                (formals |> List.map formal2type, snd rt)) 
        <| getAllClassMethods (class2name c) ast inhMap
        
    Map.empty
    |> List.fold (fun class2map c -> 
        class2map 
        |> Map.add (class2name c) (extractFromClass c))
    <| cs

let getMethodType c m methEnv = 
    methEnv
    |> Map.tryFind c 
    |> Option.bind (Map.tryFind m)

let getObjectType v objectEnv = Type "Undefined"

let typecheck objectEnv methodEnv ((loc, e) as expr) =
    match e with
    | Integer _ as n -> Type "Int", expr
    | Identifier (iloc, v) -> objectEnv v, expr