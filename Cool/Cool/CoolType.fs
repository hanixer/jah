module CoolType

open CoolAst

type Result<'a, 'Error> = 
    | Success of 'a
    | Failure of 'Error list
    
module Result =
    let bind f = function
        | Success x -> f x
        | Failure errs -> Failure errs

    let map f = function
        | Success x -> f x |> Success
        | Failure errs -> Failure errs

    let ret x = function
        | Success _ -> Success x
        | Failure errs -> Failure errs

    let rec mapList (xs:'a list) (f:'a -> Result<'b, 'e>) : Result<'b list, 'e> =
        match xs with 
        | [] -> Success []
        | x :: tail ->
            f x
            |> bind (fun y ->
                mapList tail f
                |> bind (fun ys -> y :: ys |> Success))

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
    | EqualityPrimitiveTypeError of int * Type * Type
    | AssignmentToSelf of int
    | SelfAttributeDefined of int
    | UndefinedType of Id
    | SelfTypeNotPermitted of int * string
    | PrimitiveParent of int * string
    | NoMainMethod

type Signature = string list
type MethodEnv = Map<string,Map<string,(string list * string)>>

type ObjectEnv = (string * Type) list


type MethodBody = 
    | BodyInner of (*ret type*)string * (*class name*)string * (*method name*) string
    | BodyExpr of Expr

type MethodInfo = 
    Id * (*formals*) string list * (*ultimate parent name*)string * MethodBody

type ImplMap = Map<string, MethodInfo list>
type InheritanceMap = Map<string, string>

type SemanticInfo = 
    { Attributes : Map<string, (Id * Id * Expr option) list>
      Methods : ImplMap
      InheritanceMap : InheritanceMap
      AnnotatedAst : Ast
      MainMethodBody : Expr }
    member x.ApplyImplMap c m =
        Map.find c x.Methods |> List.find (fun ((_, m2), _, _, _) -> m2 = m)

let id2str = snd

let className (Class (id, _, _)) = snd id
let parentName (Class (_, p, _)) = p |> defaultArg <| (0, "Object") |> snd

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

let allClassNamesWithStandart (Ast cs) =
    cs
    |> List.map (function | Class ((_, n), _, _) -> n)
    |> List.append
    <| ("SELF_TYPE" :: standartTypes)

let validateClassRedefinition (classes:list<string*string option>) = 
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

let ast2classes (Ast cs)  : (string * string option) list  =
    let convertToStr (Class (c, pOpt, _)) =
        id2str c, Option.map id2str pOpt
    cs 
    |> List.map convertToStr

let completeWithStandartTypes parentsOpt =
    parentsOpt
    |> List.map (fun (c, p) -> c, defaultArg p "Object")
    |> List.append
    <| List.map (fun t -> t, "Object") ("IO" :: primitiveTypes)

let validateInheritance (Ast cs) =
    let allClasses = 
        cs
        |> List.map (fun (Class ((_, n),_,_)) -> n)
        |> List.append standartTypes
    let checks =
        [ fun p -> if List.contains (snd p) allClasses then None else UndefinedType p |> Some 
          fun p -> if List.contains (snd p) primitiveTypes then PrimitiveParent p |> Some else None
          fun p -> if (snd p) = "SELF_TYPE" then SelfTypeNotPermitted (fst p, "as a parent") |> Some else None ]

    let toResult = function
        | Some er -> Failure [er]
        | None -> Success ()                 
    
    cs
    |> List.choose (fun (Class (c, pOpt, _)) -> pOpt |> Option.map (fun pid -> c, pid))
    |> List.tryPick (fun (c, p) ->
        checks
        |> List.tryPick (fun check -> check p))
    |> toResult

// TODO: add check for inheriting from primitive types
let inheritanceMapChecked ast =    
    ast
    |> ast2classes
    |> validateClassRedefinition
    |> Result.bind validateCycles
    |> Result.map completeWithStandartTypes

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
            Success ()
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
            Success ()
        else
            duplicated
            |> List.map MethodFormalsRedefined
            |> Failure
    cs
    |> List.collect validateClass
    |> report

let ast2inheritanceGraph ast =
        ast
        |> inheritanceMapChecked
        |> Result.map Graphs.edges2adjacency

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

let standartMethods =
    [   "Object",
        [   str2id "abort", [], str2id "Object"
            str2id "copy", [], str2id "SELF_TYPE"
            str2id "type_name", [], str2id "String" ]
        "IO",
        [   str2id "in_int", [], str2id "Int"
            str2id "in_string", [], str2id "String"
            str2id "out_int", [str2id "x", str2id "Int"], str2id "SELF_TYPE" 
            str2id "out_string", [str2id "x", str2id "String"], str2id "SELF_TYPE" ]
        "String",
        [   str2id "concat", [str2id "s", str2id "String"], str2id "String"
            str2id "length", [], str2id "Int"
            str2id "substr", [str2id "i", str2id "Int"; str2id "l", str2id "Int"], str2id "String" ] ]
    |> Map.ofList

let getClassMethods c ((Ast cs) as ast) : MethodSignature list =
    let methods () = 
        match tryFindClass c ast with
        | Some cl ->
            class2methodsSignatures cl
        | None -> 
            []
    match Map.tryFind c standartMethods with
    | Some m -> m
    | _ -> methods ()
            


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
    ast2classes >> completeWithStandartTypes >> Map.ofList

let validateOverridenMethods ast =
    let inhMap = ast |> inheritanceMapUnchecked
    ast 
    |> ast2inheritanceGraph 
    |> Result.map Graphs.toposort
    |> Result.map (List.collect (fun c -> getInheritedMethodsErrors c ast inhMap))
    |> Result.bind (fun errs -> 
        if List.isEmpty errs then
            Success ()
        else
            Failure errs)

let validateMethodAndFormalsTypes (Ast cs as ast) =
    let allMethods =
        cs |> List.collect methodsOf
    let allClasses = "SELF_TYPE" :: allClassNamesWithStandart ast
    let handleFormal (_, t) =
        match snd t with
        | "SELF_TYPE" -> 
            SelfTypeNotPermitted t |> Some
        | n when List.contains n allClasses |> not -> 
            UndefinedType t |> Some
        | _ -> 
            None
    let checkMethodType t errOpt =
        if Option.isNone errOpt && List.contains (snd t) allClasses |> not then
            UndefinedType t |> Some
        else errOpt
    let handleMethod (_, formals, t, _) =
        formals 
        |> List.tryPick handleFormal
        |> checkMethodType t
    let toResult = function
        | Some err -> Failure [err]
        | None -> Success ()
    
    allMethods
    |> List.tryPick handleMethod
    |> toResult

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

let getAttributesErrors c ast inhMap : TypeError option =
    let isRedefined x y =
        x <> y && attributeName x = attributeName y
    let baseAttrs = getInheritedAttributes c ast inhMap
    let selfAttrs =
        tryFindClass c ast
        |> Option.map class2attributes
        |> defaultArg
        <| []
    let attrs = List.append baseAttrs selfAttrs
    let redefined =
        attrs
        |> List.tryPick (fun x ->
            List.tryFind (isRedefined x) attrs)
        |> Option.map (fun (a, _, _) ->
                AttributeRedefined (c, a))
        |> Option.toList

    let allClasses = "SELF_TYPE" :: allClassNamesWithStandart ast
    let undefined =
        attrs
        |> List.choose (fun (_,(_, t as tid), _) ->
            if List.contains t allClasses then
                None
            else
                UndefinedType tid |> Some)
    List.append redefined undefined
    |> List.tryHead

let validateRedefinedAttributes ast = 
    let inhMap = ast |> inheritanceMapUnchecked
    let g = Graphs.edges2adjacency (Map.toList inhMap)
    let sorted = Graphs.toposort g
    let errors =
        sorted
        |> List.choose (fun c -> 
            getAttributesErrors c ast inhMap)
    
    if List.isEmpty errors then
        Success ()
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
    let accumulateMethods meth2type ((_, m), formals, rt) =
        meth2type
        |> Map.add m  
            (formals |> List.map formal2type, snd rt)
    let extractFromClass = fun c ->
        Map.empty
        |> List.fold accumulateMethods
        <| getAllClassMethods (class2name c) ast inhMap
    let extractMethInfo (m, fs, rt) =
        snd m, fs |> List.map snd        
    let stdMethods =
        standartMethods
        |> Map.fold (fun class2methods c ms ->
            let v = ms |> List.fold accumulateMethods Map.empty
            class2methods |> Map.add c v) Map.empty

    cs
    |> List.fold (fun class2map c ->
        class2map
        |> Map.add (class2name c) (extractFromClass c))
        stdMethods

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
    member x.Bind(v, f) = Result.bind f v
    member x.Return(v) = Success v
    member x.Delay(f) = f()
    member x.ReturnFrom(v) = v

    member x.Combine(res1, res2) =
        match res1, res2 with
        | Failure errs, _ -> Failure errs
        | _, Success s -> Success s
        | _, Failure errs -> Failure errs

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
        | Type ts1, SelfType ts2 ->
            isInConformance inhMap (Type ts1) (Type ts2)
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
        | SelfTypeFree -> failwith "Not Implemented"

    let r1 = prepare t1
    let r2 = prepare t2
    let minLen = min r1.Length r2.Length
    let r1 = List.take minLen r1
    let r2 = List.take minLen r2

    if t1 = t2 then t1
    else
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

let rec typecheckList typecheck = function
    | [] -> Success []
    | head :: tail ->
        result {
            let! t = typecheck head
            let! ts = typecheckList typecheck tail
            return t :: ts
        }

let validateTypeAllowed allClasses t (selfNotAllowedReason : string option) =
    if snd t = "SELF_TYPE" then
        match selfNotAllowedReason with
        | Some reason ->
            Failure [SelfTypeNotPermitted (fst t, reason)]
        | None ->
            Success ()
    elif List.contains (snd t) allClasses then
            Success ()
        else
            Failure [UndefinedType t]

type TypecheckCtx =
    { ObjectEnv : ObjectEnv
      MethodEnv : MethodEnv
      InhMap : Map<string, string>
      AllClasses : string list 
      Ast : Ast }

let rec typecheckExpr (ctx : TypecheckCtx) c (expr : Expr)  : Result<Type, TypeError> =
    let check e = typecheckExpr ctx c e
    let checkWithObjectEnv objectEnv e = typecheckExpr {ctx with ObjectEnv = objectEnv } c e
    let ret = Success
    let fail = List.singleton >> Failure

    let rec typecheckList  = function
        | [] -> Success []
        | head :: tail ->
            result {
                let! t = check head
                let! ts = typecheckList  tail
                return t :: ts
            }
    let typecheckDispatch objExpr mId argsExprs (typeCast:string option) = 
        result {
            let! objTypeInitial =
                check objExpr
            let! objType =
                match typeCast, objTypeInitial with
                | None, SelfType c' when c' = c -> Type c |> ret
                | None, t -> t |> ret
                | Some tc, t ->
                    if isInConformance ctx.InhMap t (Type tc)
                    then Success objTypeInitial
                    else Failure [StaticDispatchCast (fst mId, objTypeInitial, Type tc)]
            let! methodType =
                let c = match objType with | Type t | SelfType t -> t | _ -> "SELF_TYPE"
                getMethodType c (snd mId) ctx.MethodEnv
                |> Option.map Success
                |> defaultArg
                <| Failure [MethodNotFound (c, mId)]
            let! argsTypes = typecheckList argsExprs
            let! argsOk = 
                tryFindArgumentsError expr.Loc ctx.InhMap argsTypes (methodType |> fst |> List.map Type)
                |> Option.map (List.singleton >> Failure)
                |> defaultArg
                <| Success()
            let formalRetType = 
                match snd methodType with
                | "SELF_TYPE" -> objTypeInitial
                | _ -> methodType |> snd |> Type

            return formalRetType
        }

    let typecheckBindings bindings body = 
        let rec go objectEnv = function
            | ((loc, v), (_, t as tid), init) :: tail ->
                result {
                    do! validateTypeAllowed ctx.AllClasses tid None
                    let tLhs = if t = "SELF_TYPE" then SelfType c else Type t 
                    match init with
                    | Some initExpr ->
                        let! tRhs = checkWithObjectEnv objectEnv initExpr
                        if isInConformance ctx.InhMap tRhs tLhs
                        then
                            return! go (extendObjectEnv [v, tLhs] objectEnv) tail
                        else 
                            return! Failure [NonConformantTypes (loc, tRhs, tLhs, "let binding")]
                    | None ->
                        return! go (extendObjectEnv [v, tLhs] objectEnv) tail
                }
            | [] -> 
                checkWithObjectEnv objectEnv body
        
        go ctx.ObjectEnv bindings

    let validateCondition = function
        | Type "Bool" as t -> ret t
        | t -> ConditionBoolExpected (expr.Loc, t) |> fail

    let resultType =
        match expr.Expr with
        | Assign ((loc, v), initExpr) ->
            result {
                do! if v = "self" then Failure [AssignmentToSelf expr.Loc] else Success()
                let! tRhs = check initExpr
                let! tLhs = 
                    getObjectType v ctx.ObjectEnv 
                    |> Option.map Success
                    |> defaultArg 
                    <| (VariableNotFound >> fail) (loc, v)

                if isInConformance ctx.InhMap tRhs tLhs then
                    return tRhs
                else
                    return! 
                        NonConformantTypes (loc, tRhs, tLhs, "assignment") 
                        |> fail
            }
        | New (loc, s as tid) ->
            result {
                do! validateTypeAllowed ctx.AllClasses tid None
                if s = "SELF_TYPE" 
                then return SelfType c
                else return Type s
            }
        | DynDispatch (objExpr, mId, argsExprs) ->
            typecheckDispatch objExpr mId argsExprs None
        | SelfDispatch (mId, argsExprs) ->
            let objExpr = { Type = None; Loc = expr.Loc; Expr = Identifier (expr.Loc, "self") } 
            typecheckDispatch objExpr mId argsExprs None
        | StatDispatch (objExpr, (_, typeCast as t), mId, argsExpr) ->
            result {
                do! validateTypeAllowed ctx.AllClasses t (Some "in static dispatch")
                return! typecheckDispatch objExpr mId argsExpr (Some typeCast)
            }
        | If (cond, thenExpr, elseExpr) ->
            result {
                let! condType = 
                    check cond
                    |> Result.bind validateCondition
                        
                let! thenType = check thenExpr
                let! elseType = check elseExpr
                return joinTypes ctx.InhMap thenType elseType
            }
        | While (cond, body) ->
            result {
                let! condType = 
                    check cond
                    |> Result.bind validateCondition
                let! bodyType =
                    check body
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
        | Let (bindings, body) ->
            typecheckBindings bindings body
        | Case (toCheck, cases) ->
            result {
                let! t = check toCheck
                let! ts = Result.mapList cases (fun ((_, v), (_, t as tid), e) -> 
                    validateTypeAllowed ctx.AllClasses tid (Some "in case expression")
                    |> Result.bind (fun () -> checkWithObjectEnv (extendObjectEnv [v, Type t] ctx.ObjectEnv) e))
                
                return List.fold (joinTypes ctx.InhMap) (List.head ts) (List.tail ts)
            }
        | Isvoid e ->
            result {
                let! t = check e
                return Type "Bool"
            }
        | EQ (e1, e2) ->
            let isPrimitive = function | Type t -> List.contains t primitiveTypes | _ -> false
            result {
                let! t1 = check e1
                let! t2 = check e2
                if (isPrimitive t1 || isPrimitive t2) && (t1 <> t2)
                then 
                    return! Failure [EqualityPrimitiveTypeError (expr.Loc, t1, t2)]
                else
                    return Type "Bool"
            }
        | True | False -> 
            Type "Bool" |> ret
        | String _ -> 
            Type "String" |> ret
        | Integer _ as n -> 
            Type "Int" |> ret
        | Identifier (_, v as var) -> 
            match getObjectType v ctx.ObjectEnv with
            | Some SelfTypeFree -> c |> SelfType |> ret
            | Some t -> t |> ret
            | None -> VariableNotFound var |> fail
        | Negate e1 ->
            result {
                let! t1 = check e1
                match t1 with
                | Type "Int" -> return t1
                | _ -> return! ArithmIntExpected (e1.Loc, t1) |> fail
            }
        | Not e1 ->
            check e1
            |> Result.bind (function
                | Type "Bool" as t1 -> ret t1
                | t1 -> ArithmIntExpected (e1.Loc, t1) |> fail)
        | Plus (e1, e2) | Minus (e1, e2)
        | Times (e1, e2) | Divide (e1, e2) -> 
            result {
                let! t1 = check e1
                let! t2 = check e2
                match t1, t2 with
                | Type "Int", Type "Int" -> return t1
                | Type "Int", _ -> return! ArithmIntExpected (e2.Loc, t2) |> fail
                | _, Type "Int" -> return! ArithmIntExpected (e1.Loc, t1) |> fail
                | _, _ -> return! ArithmIntExpected (e1.Loc, t1) |> fail
            }
        | LT (e1, e2) | LE (e1, e2) ->
            result {
                let! t1 = check e1
                let! t2 = check e2
                match t1, t2 with
                | Type "Int", Type "Int" -> return Type "Bool"
                | Type "Int", _ -> return! ArithmIntExpected (e2.Loc, t2) |> fail
                | _, Type "Int" -> return! ArithmIntExpected (e1.Loc, t1) |> fail
                | _, _ -> return! ArithmIntExpected (e1.Loc, t1) |> fail
            }

    resultType
    |> Result.map (fun t -> 
        expr.Type <- Some t
        t)

let getObjectEnv c ast inhMap : ObjectEnv =
    let baseAttr = getInheritedAttributes (class2name c) ast inhMap

    class2attributes c
    |> List.append baseAttr
    |> List.map (fun (n, t, _) -> 
        let tp = snd t       
        snd n, if tp = "SELF_TYPE" then SelfType (class2name c) else Type tp)
    |> List.rev

let typecheckMethod (ctx:TypecheckCtx) (c:string) (m:Id) (formals:Formal list) rt body =
    let formalRetType = 
        match rt with
        | (_, "SELF_TYPE") -> SelfType c
        | (_, t) -> Type t
    let vs =
        formals
        |> List.map (fun (v, t) -> snd v, snd t |> Type)
    let objectEnv = 
        ctx.ObjectEnv 
        |> extendObjectEnv ["self", SelfType c]
        |> extendObjectEnv vs 
    result {
        let! retType = typecheckExpr { ctx with ObjectEnv = objectEnv } c body    
        if isInConformance ctx.InhMap retType formalRetType
        then return! Success ()
        else return! Failure [MethodReturnTypeMismatch (c, m, retType, formalRetType)]
    }

let typecheckAttributeInit (ctx : TypecheckCtx) (c : string) n tname e  = 
    let t = Type tname
    let objectEnv = 
        ctx.ObjectEnv 
        |> extendObjectEnv ["self", SelfType c]
    let ctx = 
        { ctx with ObjectEnv = objectEnv }
    typecheckExpr ctx c e
    |> Result.bind (fun t2 ->
        if t = t2 
        then Success ()
        else Failure [AttributeTypeMismatch (n, t, t2)])

let typecheckClass (ctx : TypecheckCtx) (Class (n, p, fs) as c) : Result<unit, TypeError> = 
    let objectEnv = getObjectEnv c ctx.Ast ctx.InhMap
    let cname = snd n
    let ctx = { ctx with ObjectEnv = objectEnv }
    let rec go = function
        | Method (m, formals, rt, body) :: tail -> 
            typecheckMethod ctx cname m formals rt body
            |> Result.bind (fun () -> go tail)
        | Attribute (n, (_, tname), Some e) :: tail ->
            typecheckAttributeInit ctx cname n tname e
            |> Result.bind (fun () -> go tail)
        | _ :: tail -> go tail
        | [] -> Success ()

    go fs

let typecheckAst (Ast cs as ast) =
    let methodEnv = ast2methodEnvironment ast
    let inhMap = inheritanceMapUnchecked ast
    let ctx = 
        { ObjectEnv = []
          MethodEnv = methodEnv
          Ast = ast
          InhMap = inhMap
          AllClasses = allClassNamesWithStandart ast }

    let rec go  = function
        | c :: tail -> result {
            let! v = typecheckClass ctx c 
            return! go tail }
        | [] -> Success ()
    
    go cs

let applyImplMap c m implMap =
    Map.find c implMap |> List.find (fun ((_, m2), _, _) -> m2 = m)

let getAttributes (Ast cs as ast) inhMap =
    let ownAttributes c =
        if List.contains c standartTypes
        then []
        else
            tryFindClass c ast
            |> Option.map class2attributes
            |> defaultArg
            <| []

    allClassNamesWithStandart ast
    |> List.map (fun c ->
        c,
        getInheritedAttributes c ast inhMap
        |> List.append
        <| ownAttributes c)
    |> Map.ofList

let getClass2methodsMapping (Ast cs as ast) (inhMap:Map<string, string>) =
    let merge parentMethods currMethods =
        parentMethods
        |> List.fold (fun (result, currMethods) ((_, p), _, _, _ as parentMethod) ->
            match List.tryFind (fun ((_, m), _, _, _) -> m = p) currMethods with 
            | Some meth ->
                meth :: result, List.filter ((<>) meth) currMethods
            | None ->
                parentMethod :: result, currMethods) ([], currMethods)
        |> (fun (x, y) -> List.append (List.rev x) y)

    let ownMethods c : MethodInfo list =
        match Map.tryFind c standartMethods with
        | Some ms ->
            ms
            |> List.map (fun (m,formals,rt) ->
                m, List.map formalName formals, c, BodyInner (snd rt, c, snd m))
        | None ->
            tryFindClass c ast
            |> Option.map methodsOf
            |> defaultArg 
            <| []
            |> List.map (fun (m, formals, rt, expr) ->
                m, List.map formalName formals, c, BodyExpr expr)

    let rec go currOpt (remainingClasses:Set<string>) (class2methods:Map<string, MethodInfo list>) =
        match currOpt, Seq.tryHead remainingClasses with
        | Some c, _ | None, Some c ->
            match Map.tryFind c inhMap with
            | Some p ->
                // parent is present
                match Map.tryFind p class2methods with
                | Some parentMethods ->
                    ownMethods c
                    |> merge parentMethods
                    |> Map.add c
                    <| class2methods
                    |> go None (Set.remove c remainingClasses)
                | None ->
                    go (Some p) remainingClasses class2methods
            | None ->
                // no parent
                ownMethods c
                |> Map.add c
                <| class2methods
                |> go None (Set.remove c remainingClasses)
        | _ -> 
            class2methods

    go None (allClassNamesWithStandart ast |> Set.ofList) Map.empty
    
let getEntryPointBody (Ast cs) =
    let toResult = function
        | Some body -> Success body
        | None -> Failure [NoMainMethod]

    cs
    |> List.tryPick (fun (Class ((_, c), _, fs)) ->
        if c = "Main" then
            fs
            |> List.tryPick (function 
                | Method ((_, "main"), _, _, body) -> Some body
                | _ -> None)
        else None)
    |> toResult


let analyze ast =
    result {
        do! validateInheritance ast
        let! mainMethodBody = getEntryPointBody ast
        let! inhMapList = inheritanceMapChecked ast
        let inhMap = Map.ofList inhMapList
        do! validateMethodRedefinition ast
        do! validateMethodFormalsRedefinition ast
        do! validateMethodAndFormalsTypes ast
        do! validateOverridenMethods ast
        do! validateRedefinedAttributes ast
        do! typecheckAst ast
        return {
            Attributes = getAttributes ast inhMap
            Methods = getClass2methodsMapping ast inhMap
            InheritanceMap = inhMap
            AnnotatedAst = ast
            MainMethodBody = mainMethodBody
        }
    }