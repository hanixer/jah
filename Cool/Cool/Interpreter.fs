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
    | AbortCalled

exception InterpreterException of RuntimeError

let runtimeErr e = InterpreterException(e) |> raise

let applyStore (store:Store) loc = 
    if store.Dict.ContainsKey(loc) then
        Some store.Dict.[loc]
    else
        None

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
        VoidVal


let rec evaluate (semInfo:SemanticInfo) (so:Value) (store:Store) env (expr:Expr) : Value =
    let eval so env expr = evaluate semInfo so store env expr
    match expr.Expr with
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
    | DynDispatch (objExpr, (_, meth), args) ->
        let argVals =
            args |> List.map (fun e -> eval so env e)
        let objVal = eval so env objExpr
        let objType = typeOf objVal
        let locs = 
            argVals
            |> List.map (newLocationInit store)
        let _, formals, _, body = semInfo.ApplyImplMap objType meth
        let newEnv =
            List.zip formals locs
            |> List.fold (fun acc (f, l) -> Map.add f l acc) (valueAttributes objVal)
        
        match body with
        | BodyExpr bodyExpr -> eval objVal newEnv bodyExpr
        | _ -> StringVal (0, "internal")
        
        
