module Interpreter

open CoolAst
open CoolType

type Loc = int

type Value =
    | IntVal of int
    | BoolVal of bool
    | StringVal of int * string
    | ObjectVal of string * Map<string, Loc>
    | VoidVal


type Env = Map<string, Loc>

type Store = 
    { mutable NewLoc : Loc  
      Dict : System.Collections.Generic.Dictionary<Loc, Value> }

type RuntimeError =
    | Uninitilized
    | LocationNotFound of int * Loc
    | VariableNotFound of int * string
    | DispatchOnVoid of int
    | AbortCalled
    | BlockExpressionIsEmpty of int
    | CaseWithoutMatchingBranch of int * Value
    | CaseOnVoid of int

exception InterpreterException of RuntimeError

let runtimeErr e = InterpreterException(e) |> raise

let applyStore (store:Store) loc = 
    if store.Dict.ContainsKey(loc) then
        Some store.Dict.[loc]
    else
        None

let applyEnv = Map.tryFind
let extendEnv = Map.add

let newLocation (store:Store) = 
    let loc = store.NewLoc
    store.NewLoc <- store.NewLoc + 1
    loc

let setLocationValue (store:Store) loc v =
    store.Dict.[loc] <- v

let newLocationInit (store:Store) v =
    let loc = newLocation store
    setLocationValue store loc v
    loc

let defaultValue = function
    | "Int" -> IntVal 0
    | "Bool" -> BoolVal false
    | "String" -> StringVal (0, "")
    | _ -> VoidVal

let typeOf = function
    | IntVal _ -> "Int"
    | BoolVal _ ->  "Bool" 
    | StringVal _ -> "String"    
    | ObjectVal (t, _) -> t
    | _ -> "Object"

let valueAttributes = function
    | ObjectVal (_, attrs) -> attrs
    | _ -> Map.empty

let internalMethod c m object args =
    match c with
    | "Object" ->
        match m with
        | "abort" ->
            runtimeErr AbortCalled
        | "copy" ->
            object
        | "type_name" ->
            let t = typeOf object
            StringVal (t.Length, t)
        | _ -> failwithf "There is no method %s for Object" m
    | "IO" ->
        match m with
        | "out_string" ->
            let s = match args with | [StringVal (_, s')] -> s' | _ -> ""
            s.Replace("\\n", "\n").Replace("\\t", "\t") |> printfn "%s"
            object
        | _ -> failwithf "There is no method %s for IO" m
    | _ ->
        c |> failwithf "Unknown class %s"

let tryFindCaseBranch inhMap (ancestors : (Id * Id * Expr) list) c : (Id * Id * Expr) option = 
    let rec path x =
        match Map.tryFind x inhMap with
        | Some p ->
            x :: path p
        | None -> [x]
    let pathFinal = path c
    
    let distances =
        ancestors
        |> List.choose (fun (_, (_, anc), _ as x) ->
            pathFinal 
            |> List.tryFind ((=) anc)
            |> Option.map (fun idx -> x, idx))
    
    if distances.IsEmpty then
        None
    else
        distances
        |> List.minBy snd
        |> fst
        |> Some

let rec evaluate (semInfo:SemanticInfo) (so:Value) (store:Store) env (expr:Expr) : Value =
    let eval so env expr = evaluate semInfo so store env expr

    let evalDispatch objVal objType args meth =
        if objVal = VoidVal then
            DispatchOnVoid expr.Loc |> runtimeErr
            
        let argVals =
            args |> List.map (fun e -> eval so env e)
        let locs = 
            argVals
            |> List.map (newLocationInit store)
        let _, formals, _, body = semInfo.ApplyImplMap objType meth
        let newEnv =
            List.zip formals locs
            |> List.fold 
                (fun acc (f, l) -> Map.add f l acc) 
                (valueAttributes objVal)
        
        match body with
        | BodyExpr bodyExpr -> 
            eval objVal newEnv bodyExpr
        | BodyInner (rt, c, m) -> 
            try
                internalMethod c m objVal argVals
            with 
            | e -> 
                printfn "Exception in %A" expr
                reraise()

    match expr.Expr with
    | Assign (lhs, rhs) ->
        match applyEnv (snd lhs) env with
        | Some loc ->
            let rhsVal = eval so env rhs 
            setLocationValue store loc rhsVal
            rhsVal
        | None ->
            VariableNotFound lhs |> runtimeErr
    | Let (bindings, body) ->
        let envNew =
            bindings
            |> List.fold (fun envAcc ((_, v), (_, t), rhsOpt) ->
                let rhsVal =                 
                    match rhsOpt with
                    | Some rhs ->
                        eval so env rhs
                    | None ->
                        defaultValue t
                let loc = newLocationInit store rhsVal
                extendEnv v loc envAcc) env
        eval so envNew body
    | Block exprs ->
        if exprs.IsEmpty then
            BlockExpressionIsEmpty expr.Loc |> runtimeErr
        
        exprs
        |> List.fold (fun acc elt ->
            eval so env elt) VoidVal
    | True -> BoolVal true
    | False -> BoolVal false
    | Identifier v ->
        match snd v with
        | "self" -> so
        | _ -> 
            match applyEnv (snd v) env with
            | Some loc -> 
                match applyStore store loc with
                | Some value -> value
                | None -> LocationNotFound (expr.Loc, loc) |> runtimeErr 
            | None ->
                VariableNotFound v |> runtimeErr
    | String s -> StringVal (s.Length, s)
    | Integer n -> IntVal n
    | New (_, t) ->
        let t' = if t = "SELF_TYPE" then typeOf so else t
        let attrs = 
            semInfo.Attributes.[t']
            |> List.map (fun ((_, v), (_, t), e) -> 
                let loc = newLocation store
                defaultValue t |> setLocationValue store loc 
                v, loc, e)
        let object = 
            t', attrs 
                |> List.map (fun (v,t,e) -> v,t) 
                |> Map.ofList
        let objVal = ObjectVal object
        attrs 
        |> List.iter (fun (v, loc, eOpt) ->
            eOpt
            |> Option.iter (fun initExpr -> 
                eval objVal (object |> snd) initExpr
                |> setLocationValue store loc))
        object |> ObjectVal
    | Case (target, cases) ->
        let value = eval so env target
        if value = VoidVal then
            CaseOnVoid expr.Loc |> runtimeErr

        let typ = typeOf value
        match tryFindCaseBranch semInfo.InheritanceMap cases typ with
        | Some (v, t, body) ->
            let loc = newLocationInit store value
            eval so (extendEnv (snd v) loc env) body
        | None ->
            CaseWithoutMatchingBranch (expr.Loc, value) |> runtimeErr

    | DynDispatch (objExpr, (_, meth), args) ->
        let objVal = eval so env objExpr
        let objType = typeOf objVal
        evalDispatch objVal objType args meth
    | SelfDispatch ((_, meth), args) ->
        let objType = typeOf so
        evalDispatch so objType args meth
    | StatDispatch (objExpr, (_, t), (_, meth), args) ->
        let objVal = eval so env objExpr
        evalDispatch objVal t args meth

    | _ ->
        failwithf "Unknown expression %A" expr