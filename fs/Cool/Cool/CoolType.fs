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
    | MethodInheritedFormalTypeDiffer of (*class*)string * (*method*)Id * (*formal name*) Id * (*current type*)Id * (*expected type*)Id
    | MethodInheritedReturnTypeDiffer of string * Id * Id * Id
    | AttributeRedefined of string * Id
    | VariableNotFound of Id
    | ArithmIntExpected of int * Type
    | AttributeTypeMismatch of Id * Type * Type
    | MethodReturnTypeMismatch of (*class*) string * (*method*) Id * (*actual*) Type * (*expected*) Type
    | NonConformantTypes of int * (*actual*)Type * (*expected*)Type * string
    | MethodNotFound of (*class*)string * Id
    | DispatchWrongNumberOfArgs of (*loc*)int * (*actual*)int * (*expected*)int
    | DispatchArgumentTypeMismatch of (*loc*)int * (*number*) int * (*actual*) Type * (*expected*) Type
    | StaticDispatchCast of (*loc*)int * (*actual*) Type * (*expected*) Type
    | ConditionBoolExpected of int * Type
    | SequenceExprHasNoSubexpressions of int

type Signature = string list
type MethodEnv = Map<string,Map<string,(string list * string)>>

type ObjectEnv = (string * string) list

// TODO: add checks for whether the type is declared
//       if SELF_TYPE is used whether it is permitted to use
// Uses of SELF_TYPE:
// new SELF_TYPE, 
// as the return type of a method, 
// as the declared type of a let variable, or 
// as the declared type of an attribute. 

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

let bindRes f = function
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
    |> bindRes validateCycles
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

let inheritanceMapUnchecked : (Ast -> Map<string,string>)= 
    classesFromAst >> completeWithStandartTypes >> Map.ofList

let validateRedefinedMethods ast =
    let inhMap = ast |> inheritanceMapUnchecked
    ast 
    |> ast2inheritanceGraph 
    |> mapRes Graphs.toposort
    |> mapRes (List.collect (fun c -> getInheritedMethodsErrors c ast inhMap))
    |> bindRes (fun errs -> 
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

let ast2methodEnvironment (Ast cs as ast) : MethodEnv =
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

let getMethodType c m (methEnv:MethodEnv) : MethodType option = 
    methEnv
    |> Map.tryFind c 
    |> Option.bind (Map.tryFind m)

let extendObjectEnv varTypes (objectEnv:ObjectEnv) : ObjectEnv =
    varTypes
    |> List.fold (fun oe (v, t) -> (v, t) :: oe) objectEnv

let getObjectType v (objectEnv:ObjectEnv) = 
    objectEnv
    |> List.tryFind (fun x -> (fst x) = v)
    |> Option.map snd

type ResultBuilder() =
    member x.Bind(v, f) = bindRes f v
    member x.Return(v) = Success v
    member x.Delay(f) = f()
    member x.ReturnFrom(v) = v

let result = ResultBuilder()

// inhMap must include basic types
let rec isInConformance inhMap t1 t2 =
    let rec isInheritFrom ts1 ts2 =
        match Map.tryFind ts1 inhMap with
        | Some p when p = ts2 -> true
        | Some p -> isInheritFrom p ts2
        | None -> false
    if t1 = t2 
    then true
    else 
        match t1, t2 with
        | Type ts1, Type ts2 ->
            isInheritFrom ts1 ts2
        | SelfType ts1, Type ts2 ->
            isInConformance inhMap (Type ts1) t2
        | _, _ -> 
            false

let joinTypes inhMap t1 t2 : Type =
    let rec makeRoute t =
        match Map.tryFind t inhMap with
        | None -> [t]
        | Some p -> t :: (makeRoute p)
    let prepare t = 
        match t with 
        | SelfType s | Type s -> s |> makeRoute |> List.rev

    printfn "inhMap joinTypes = %A" inhMap

    let r1 = prepare t1
    let r2 = prepare t2
    let minLen = min r1.Length r2.Length
    let r1 = List.take minLen r1
    let r2 = List.take minLen r2

    List.zip r1 r2
    |> List.takeWhile (fun (x, y) ->x = y)
    |> List.last
    |> fst 
    |> Type

//func : ts1 -> ts2 -> class -> 
let tryFindArgumentsError (loc:int) inhMap (argsTypes:Type list) (formalsTypes:Type list) : TypeError option =
    let rec go = function
        | (i, argType, formalType) :: tail ->
            if isInConformance inhMap argType formalType
            then go tail
            else
                DispatchArgumentTypeMismatch (loc, i, argType, formalType) 
                |> Some
        | [] -> None

    if argsTypes.Length <> formalsTypes.Length
    then 
        DispatchWrongNumberOfArgs (loc, argsTypes.Length, formalsTypes.Length) 
        |> Some
    else
        List.zip3 (seq{1..argsTypes.Length} |> Seq.toList) argsTypes formalsTypes
        |> go

(*
    | Let of (Id * Id * Expr option) list * Expr
    | Case of Expr * (Id * Id * Expr) list
    | Isvoid of Expr
    | EQ of Expr * Expr
*)

let rec typecheckList typecheck = function
    | [] -> Success []
    | head :: tail ->
        result {
            let! t = typecheck head
            let! ts = typecheckList typecheck tail
            return t :: ts
        }

let rec typecheck2 c objectEnv methodEnv (expr : Expr) inhMap : Result<Type, TypeError> =
    let typecheck e = typecheck2 c objectEnv methodEnv e inhMap
    let typecheckOE objectEnv e = typecheck2 c objectEnv methodEnv e inhMap
    // func : Expr list -> Result<Type list, TypeError>
    let ret = Success
    let fail = List.singleton >> Failure
    let rec typecheckList  = function
        | [] -> Success []
        | head :: tail ->
            result {
                let! t = typecheck head
                let! ts = typecheckList  tail
                return t :: ts
            }
    let tcdp objExpr mId argsExprs (typeCast:string option) = 
        result {
            let! objTypeInitial =
                typecheck objExpr
            let! objType =
                match typeCast, objTypeInitial with
                | None, SelfType c' when c' = c -> Type c |> ret
                | None, t -> t |> ret
                | Some tc, t ->
                    if isInConformance inhMap t (Type tc)
                    then Success objTypeInitial
                    else Failure [StaticDispatchCast (fst mId, objTypeInitial, Type tc)]
            let! methodType =
                let c = match objType with | Type t | SelfType t -> t
                getMethodType c (snd mId) methodEnv
                |> Option.map Success
                |> defaultArg
                <| Failure [MethodNotFound (c, mId)]
            let! argsTypes = typecheckList argsExprs
            let! argsOk = 
                tryFindArgumentsError expr.Loc inhMap argsTypes (methodType |> fst |> List.map Type)
                |> Option.map (List.singleton >> Failure)
                |> defaultArg
                <| Success()
            let formalRetType = 
                match snd methodType with
                | "SELF_TYPE" -> objType 
                | _ -> methodType |> snd |> Type

            return formalRetType
        }

    // let typecheckBinding ((loc, v), (_, t), init) body =
    //     result {
    //         match init with
    //         | Some initExpr ->
    //             let tLhs = if t = "SELF_TYPE" then SelfType c else Type t 
    //             let! tRhs = typecheck initExpr
    //             if isInConformance inhMap tRhs tLhs
    //             then
    //                 // let! tBody = typecheckOE (extendObjectEnv [tLhs c
    //                 return Success (Type "")

    //             else 
    //                 return! Failure [NonConformantTypes (loc, tRhs, tLhs, "let binding")]
    //             return Type ""
    //     }
    let validateCondition = function
        | Type "Bool" as t -> ret t
        | t -> ConditionBoolExpected (expr.Loc, t) |> fail

    let resultType =
        match expr.Expr with
        | Assign ((loc, v), initExpr) ->
            result {
                let! tRhs = typecheck initExpr
                let! tLhs = 
                    getObjectType v objectEnv 
                    |> Option.map (Type >> Success)
                    |> defaultArg 
                    <| (VariableNotFound >> fail) (loc, v)

                if isInConformance inhMap tRhs tLhs then
                    return tRhs
                else
                    return! 
                        NonConformantTypes (loc, tRhs, tLhs, "assignment") 
                        |> fail
            }
        | New (loc, s) ->
            if s = "SELF_TYPE" 
            then SelfType c |> ret
            else Type s |> ret
        | DynDispatch (objExpr, mId, argsExprs) ->
            tcdp objExpr mId argsExprs None
        | SelfDispatch (mId, argsExprs) ->
            tcdp { Type = None; Loc = expr.Loc; Expr = Identifier (expr.Loc, "self") } mId argsExprs None
        | StatDispatch (objExpr, (_, typeCast), mId, argsExpr) ->
            tcdp objExpr mId argsExpr (Some typeCast)
        | If (cond, thenExpr, elseExpr) ->
            result {
                let! condType = 
                    typecheck cond
                    |> bindRes validateCondition
                        
                let! thenType = typecheck thenExpr
                let! elseType = typecheck elseExpr
                return joinTypes inhMap thenType elseType
            }
        | While (cond, body) ->
            result {
                let! condType = 
                    typecheck cond
                    |> bindRes validateCondition
                let! bodyType =
                    typecheck body
                return Type "Object"                    
            }
        | Block exprs -> 
            result {
                let! types = typecheckList exprs
                return! 
                    List.tryLast types
                    |> Option.map Success
                    |> defaultArg
                    <| Failure [SequenceExprHasNoSubexpressions expr.Loc]
            }
        // | Let (bindings, body) ->

        | True | False -> 
            Type "Bool" |> ret
        | String _ -> 
            Type "String" |> ret
        | Integer _ as n -> 
            Type "Int" |> ret
        | Identifier (_, v as var) -> 
            match getObjectType v objectEnv with
            | Some "SELF_TYPE" -> c |> SelfType |> ret
            | Some t -> t |> Type |> ret
            | None -> VariableNotFound var |> fail
        | Negate e1 ->
            result {
                let! t1 = typecheck e1
                match t1 with
                | Type "Int" -> return t1
                | _ -> return! ArithmIntExpected (e1.Loc, t1) |> fail
            }
        | Not e1 ->
            typecheck e1
            |> bindRes (function
                | Type "Bool" as t1 -> ret t1
                | t1 -> ArithmIntExpected (e1.Loc, t1) |> fail)
        | Plus (e1, e2) | Minus (e1, e2)
        | Times (e1, e2) | Divide (e1, e2) -> 
            result {
                let! t1 = typecheck e1
                let! t2 = typecheck e2
                match t1, t2 with
                | Type "Int", Type "Int" -> return t1
                | Type "Int", _ -> return! ArithmIntExpected (e2.Loc, t2) |> fail
                | _, Type "Int" -> return! ArithmIntExpected (e1.Loc, t1) |> fail
                | _, _ -> return! ArithmIntExpected (e1.Loc, t1) |> fail
            }
        | LT (e1, e2) | LE (e1, e2) ->
            result {
                let! t1 = typecheck e1
                let! t2 = typecheck e2
                match t1, t2 with
                | Type "Int", Type "Int" -> return Type "Bool"
                | Type "Int", _ -> return! ArithmIntExpected (e2.Loc, t2) |> fail
                | _, Type "Int" -> return! ArithmIntExpected (e1.Loc, t1) |> fail
                | _, _ -> return! ArithmIntExpected (e1.Loc, t1) |> fail
            }
        | _ ->
            Failure []

    resultType
    |> mapRes (fun t -> 
        expr.Type <- Some t
        t)

let getObjectEnv c ast inhMap : ObjectEnv =
    let baseAttr = getInheritedAttributes (class2name c) ast inhMap

    class2attributes c
    |> List.append baseAttr
    |> List.map (fun (n, t, _) -> snd n, snd t)
    |> List.rev

let typecheckMethod objectEnv methodEnv (c:string) (m:Id) formals rt body inhMap =
    let formalRetType = 
        match rt with
        | (_, "SELF_TYPE") -> SelfType c
        | (_, t) -> Type t
    let vs =
        formals
        |> List.map (fun (v, t) -> snd v, snd t)
    let objectEnv = 
        objectEnv 
        |> extendObjectEnv ["self", "SELF_TYPE"]
        |> extendObjectEnv vs 
    result {
        let! retType = typecheck2 c objectEnv methodEnv body inhMap    
        if isInConformance inhMap retType formalRetType
        then return! Success ()
        else return! Failure [MethodReturnTypeMismatch (c, m, retType, formalRetType)]
    }

let typecheckAttributeInit objectEnv methodEnv (c:string) n tname e (inhMap:Map<string, string>) = 
    let t = Type tname
    typecheck2 c objectEnv methodEnv e inhMap
    |> bindRes (fun t2 ->
        if t = t2 
        then Success ()
        else Failure [AttributeTypeMismatch (n, t, t2)])

let typecheckClass methodEnv (Class (n, p, fs) as c) ast inhMap : Result<unit, TypeError> = 
    let objectEnv = getObjectEnv c ast inhMap
    let cname = snd n
    let rec go = function
        | Method (m, formals, rt, body) :: tail -> 
            typecheckMethod objectEnv methodEnv cname m formals rt body inhMap
        | Attribute (n, (_, tname), Some e) :: tail ->
            typecheckAttributeInit objectEnv methodEnv cname n tname e inhMap
            |> bindRes (fun () -> go tail)
        | _ :: tail -> go tail
        | [] -> Success ()

    go fs

let typecheckAst (Ast cs as ast) =
    let methodEnv = ast2methodEnvironment ast
    let inhMap = inheritanceMapUnchecked ast
    let rec go  = function
        | c :: tail -> result {
            let! v = typecheckClass methodEnv c ast inhMap
            return! go tail }
        | [] -> Success ()
    
    go cs