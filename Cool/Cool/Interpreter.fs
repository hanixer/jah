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
    | DivisionByZero of int
    | SubstringOutOfRange of string * int * int

exception InterpreterException of RuntimeError

let runtimeErr e = InterpreterException(e) |> raise

let applyStore (store:Store) loc = 
    if store.Dict.ContainsKey(loc) then
        Some store.Dict.[loc]
    else
        None

let applyEnv v env = 
    match Map.tryFind (snd v) env with
    | Some loc -> loc        
    | None -> VariableNotFound v |> runtimeErr

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

let internalMethod c m objVal args =
    match c with
    | "Object" ->
        match m with
        | "abort" ->
            runtimeErr AbortCalled
        | "copy" ->
            objVal
        | "type_name" ->
            let t = typeOf objVal
            StringVal (t.Length, t)
        | _ -> failwithf "There is no method %s for Object" m

    | "IO" ->
        match m with
        | "out_string" ->
            let s = match args with | [StringVal (_, s')] -> s' | _ -> ""
            s.Replace("\\n", "\n").Replace("\\t", "\t") |> printf "%s"
            objVal
        | "out_int" ->
            let n = match args with | [IntVal m] -> m | _ -> 0
            printf "%d" n
            objVal
        | "in_string" ->
            let s = System.Console.ReadLine()
            let lenStr = 
                if s.IndexOf(0 |> char) >= 0 then
                    (0, "")
                else
                    (s.Length, s)
            StringVal lenStr
        | "in_int" ->
            let line = System.Console.ReadLine()            
            let v = 
                try
                    int32 line
                with
                | _ -> 0
            IntVal v                
        | _ -> failwithf "There is no method %s for IO" m

    | "String" ->
        match m with
        | "length" ->
            match objVal with
            | StringVal (l, _) -> IntVal l
            | _ -> IntVal 0
        | "concat" ->
            match objVal, args with
            | StringVal (_, s1), [ StringVal (_, s2) ] ->
                let s = s1 + s2
                StringVal (s.Length, s)
            | _ -> StringVal (0, "")
        | "substr" ->
            match objVal, args with
            | StringVal (l, s), [ IntVal index; IntVal length ] ->
                try
                    let newStr = s.Substring(index, length) 
                    StringVal (newStr.Length, newStr)
                with
                | :? System.ArgumentOutOfRangeException ->
                    SubstringOutOfRange (s, index, length) |> runtimeErr
            | _ -> StringVal (0, "")
        | _ -> failwithf "There is no method %s for String" m

    | _ ->
        c |> failwithf "Unknown internal class %s"

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

let createNewObject semInfo store enclosingType objectType eval = 
    let t' = 
        match objectType with
        | "SELF_TYPE" -> enclosingType
        | _ -> objectType
    let initAttribute ((_, v), (_, t), e) =
        let loc = newLocation store
        defaultValue t |> setLocationValue store loc 
        v, loc, e
    let attrs = 
        semInfo.Attributes.[t']
        |> List.map initAttribute
    let ob = 
        t', attrs 
            |> List.map (fun (v,loc,e) -> v,loc) 
            |> Map.ofList
    let objVal = ObjectVal ob
    attrs 
    |> List.iter (fun (v, loc, eOpt) ->
        eOpt
        |> Option.iter (fun initExpr -> 
            eval objVal (ob |> snd) initExpr
            |> setLocationValue store loc))
    ob

let rec evaluate (semInfo:SemanticInfo) (store:Store) (so:Value) env (expr:Expr) : Value =
    let eval so env expr = evaluate semInfo store so env expr

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
            internalMethod c m objVal argVals

    let evalArithmetic op e1 e2 =
        match eval so env e1, eval so env e2 with
        | IntVal n1, IntVal n2 ->
            op n1 n2 |> IntVal
        | _ -> failwith "integer is expected in arithmetic operation"

    match expr.Expr with
    | Assign (lhs, rhs) ->
        let loc = applyEnv lhs env
        let rhsVal = eval so env rhs 
        setLocationValue store loc rhsVal
        rhsVal
    | Let (bindings, body) ->
        let accumulateEnv envAcc ((_, v), (_, t), rhsOpt) =
            let rhsVal =                 
                match rhsOpt with
                | Some rhs ->
                    eval so env rhs
                | None ->
                    defaultValue t
            let loc = newLocationInit store rhsVal
            extendEnv v loc envAcc
        let envNew =
            bindings
            |> List.fold accumulateEnv env

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
            let loc = applyEnv v env
            match applyStore store loc with
            | Some value -> value
            | None -> LocationNotFound (expr.Loc, loc) |> runtimeErr 
    | String s -> StringVal (s.Length, s)
    | Integer n -> IntVal n
    | New (_, t) ->
        createNewObject semInfo store (typeOf so) t eval
        |> ObjectVal
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

    | If (condExpr, thenExpr, elseExpr) ->        
        match eval so env condExpr with
        | BoolVal true ->
            eval so env thenExpr
        | BoolVal false ->
            eval so env elseExpr
        | _ ->
            failwith "Condition of if does not evaluate to Bool"
    | While (condExpr, body) ->
        match eval so env condExpr with
        | BoolVal true ->
            eval so env body |> ignore
            eval so env expr
        | BoolVal false ->
            VoidVal
        | _ -> 
            failwith "Condition of while does not evaluate to Bool"
    | Not e ->
        match eval so env e with
        | BoolVal b -> BoolVal (not b)
        | _ -> failwith "bool is expected in Not"
    | Negate e ->
        match eval so env e with
        | IntVal i -> IntVal (-i)
        | _ -> failwith "wrong type in negate"
    | Plus (e1, e2) -> evalArithmetic (+) e1 e2
    | Minus (e1, e2) -> evalArithmetic (-) e1 e2
    | Times (e1, e2) -> evalArithmetic (*) e1 e2
    | Divide (e1, e2) ->
        try
            evalArithmetic (/) e1 e2
        with
        | :? System.DivideByZeroException ->
            DivisionByZero expr.Loc |> runtimeErr
    | EQ (e1, e2) ->
        if eval so env e1 = eval so env e2 then
            BoolVal true
        else 
            BoolVal false
    | LT (e1, e2) ->
        match eval so env e1, eval so env e2 with
        | IntVal n1, IntVal n2 -> n1 < n2 |> BoolVal
        | StringVal (_, s1), StringVal (_, s2) -> s1 < s2 |> BoolVal
        | BoolVal b1, BoolVal b2 -> b1 < b2 |> BoolVal
        | _ -> BoolVal false
    | LE (e1, e2) ->
        match eval so env e1, eval so env e2 with
        | IntVal n1, IntVal n2 -> n1 <= n2 |> BoolVal
        | StringVal (_, s1), StringVal (_, s2) -> s1 <= s2 |> BoolVal
        | BoolVal b1, BoolVal b2 -> b1 <= b2 |> BoolVal
        | _ -> BoolVal false
    | Isvoid e ->
        match eval so env e with
        | VoidVal -> BoolVal true
        | _ -> BoolVal false

let execute (semInfo : SemanticInfo) =
    let store = 
        { NewLoc = 0
          Dict = System.Collections.Generic.Dictionary<Loc, Value>() }
    let eval = evaluate semInfo store
    let mainObject = createNewObject semInfo store "Main" "Main" eval
    try
        eval (ObjectVal mainObject) (snd mainObject) semInfo.MainMethodBody
        |> Success
    with
    | InterpreterException rerr ->
        Failure [rerr]