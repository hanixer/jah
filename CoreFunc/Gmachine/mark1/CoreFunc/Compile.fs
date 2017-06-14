module Compile 

open Language
open Util

type TiStack = Addr list
type TiDump = TiStack list

type Primitive = Neg | Add | Sub | Mul | Div

type Node =
    | NAp of Addr * Addr
    | NSc of Name * Name list * CoreExpr
    | NNum of int
    | NInd of Addr
    | NPrimitive of Name * Primitive
    | NData of int * (Addr list)

type TiHeap = Heap<Node>

type TiGlobals = (Name * Addr) list

type TiStats = int

type TiState =  
    { Stack : TiStack 
      Dump : TiDump
      Heap : TiHeap 
      Globals : TiGlobals
      Stats : TiStats }

let initialTiDump = []

let tiStatInitial = 0
let tiStatIncSteps s = s + 1
let tiStatGetSteps s = s
let applyToStats func state =
    { state with Stats = func state.Stats}

let extraPreludeDefs = []

let primitives = [ 
    "negate", Neg
    "+", Add
    "-", Sub
    "*", Mul 
    "/", Div
    ]

let envTryLookup env name =
    List.tryFind (fst >> ((=) name)) env

let allocateSc heap (name, args, body) =
    let heap', addr = heapAlloc heap (NSc (name, args, body))
    heap', (name, addr)

let allocatePrim heap (name, prim) =
    let heap', addr = heapAlloc heap (NPrimitive (name, prim))
    heap', (name, addr)

let buildInitialHeap scDefns = 
    let heap1, scAddrs = mapAccumul allocateSc Map.empty scDefns
    let heap2, primAddrs = mapAccumul allocatePrim heap1 primitives
    heap2, scAddrs |>List.append<| primAddrs

let compile program = 
    let scDefs = program |>List.append<| preludeDefs |>List.append<| extraPreludeDefs
    let initialHeap, globals = buildInitialHeap scDefs
    let addressOfMain = 
        match List.tryFind (fst >> ((=) "main")) globals with
        | Some (_, x) -> x
        | _ -> failwithf "main is not defined %A" globals
    let initialStack = [addressOfMain]

    {   Stack = initialStack
        Dump = initialTiDump
        Heap = initialHeap
        Globals = globals
        Stats = tiStatInitial }

let doAdmin state = applyToStats tiStatIncSteps state

let rec isDataNode heap addr =
    match heapLookup heap addr with
    | NNum n -> true
    | NInd addr2 -> isDataNode heap addr2
    | _ -> false

let tiFinal = function
    | { Stack = [soleAddr]; Heap = heap; Dump = [] } ->
        isDataNode heap soleAddr
    | { Stack = [] } -> 
        failwith "Empty stack!"
    | _ -> 
        false

let instantiateConstr tag arity heap env =
    failwith "Can't instantiate constr now"

let rec instantiate expr heap env =
    let heap', addr =
        match expr with
        | ENum n ->
            heapAlloc heap (NNum n)
        | EAp (e1, e2) ->
            let heap1, a1 = instantiate e1 heap env
            let heap2, a2 = instantiate e2 heap1 env
            heapAlloc heap2 (NAp (a1, a2))
        | EVar name ->
            match List.tryFind (fst >> ((=) name)) env with
            | Some (_, x) ->
                heap, x
            | None -> failwithf "Variable %s is not found in instantiation!(%A)" name env
        | ELet (isrec, defs, body) ->
            instantiateLet isrec defs body heap env
        | EConstr (a, b) ->
            instantiateConstr a b heap env
        | ECase _ -> 
            failwith "Can't instantiate case expression!"
        | ELam _ ->
            failwith "Con't instantiate lambda expression"
    heap', addr
    

and instantiateLet isrec defs body heap env =
    match isrec with
    | true ->
        let allocateDummyArg (env, heap) (arg, _) =
            let heap', addr = heapAlloc heap (NNum 1)
            (arg, addr) :: env, heap'

        let envDummy, heapDummy = List.fold allocateDummyArg (env, heap) defs 
        
        let allocateDef heap (name, expr) =
            let heap', addr = instantiate expr heap envDummy
            heap', (name, addr)

        let heapWithReal, envWithReal = mapAccumul allocateDef heapDummy defs

        let envCreatedVars = List.take (envDummy.Length - env.Length) envDummy

        let substituteDummyByReal heap (name, dummyAddr) =
            let _, realAddr = List.find (fst >> ((=) name)) envWithReal
            let realNode = heapLookup heapWithReal realAddr
            let heap' = heapUpdate heap dummyAddr realNode
            heapRemove heap' realAddr

        let heapFinal = List.fold substituteDummyByReal heapWithReal envCreatedVars

        instantiate body heapFinal envDummy
    | false ->
        let allocateDef heap (name, expr) =
            let heap', addr = instantiate expr heap env
            heap', (name, addr)

        let newHeap, allocatedDefs = mapAccumul allocateDef heap defs
        let newEnv = allocatedDefs |>List.append<| env
        instantiate body newHeap newEnv

let instantiateAndUpdate expr updAddr heap env =
    match expr with
    | EAp (e1, e2) ->
        let heap1, a1 = instantiate e1 heap env
        let heap2, a2 = instantiate e2 heap1 env
        heapUpdate heap2 updAddr (NAp (a1, a2))
    | ENum n ->
        heapUpdate heap updAddr (NNum n)
    | EVar name ->
        match envTryLookup env name with
        | Some (_, addr) ->
            heapUpdate heap updAddr (NInd addr)
        | None ->
            failwithf "Variable %s is not found!" name
    | ELet (isrec, defs, body) ->
        let heap1, addr' = instantiateLet isrec defs body heap env
        let node = heapLookup heap1 addr'
        let heap2 = heapUpdate heap1 updAddr node
        let heap3 = heapRemove heap2 addr'
        heap3

    | _ ->
        failwith "Con't instantiate lambda expression, case or constructor"

let getArgs heap stack =
    let rec getArg addr = 
        match heapLookup heap addr with
        | NAp (func, arg) -> arg
        | NInd addr -> getArg addr
        | n -> failwithf "NAp node is expected, but got [%d ; %A] stack %A"  addr n stack
    List.map getArg (List.tail stack)

let getRootAddr stack argNames =
    List.item (List.length argNames) stack 

let rec getNodeNotInderect heap addr =
    match heapLookup heap addr with
    | NInd addr' -> getNodeNotInderect heap addr'
    | n -> n

let evalNodeOnNewStack state nodeAddr =
    { state with
        Stack = [nodeAddr] 
        Dump = state.Stack :: state.Dump }

let primNeg (state : TiState) =
    let argAddr = getArgs state.Heap (List.take 2 state.Stack) |> List.head
    match isDataNode state.Heap argAddr with
    | true -> 
        let rootAddr = List.item 1 state.Stack
        match getNodeNotInderect state.Heap argAddr with
        | NNum n -> 
            let newHeap = heapUpdate state.Heap rootAddr (NNum -n)
            let newStack = List.tail state.Stack
            { state with
                Stack = newStack
                Heap = newHeap }
        | _ -> failwith "number node is expected"
    | false ->
        evalNodeOnNewStack state argAddr

let primArith (state : TiState) (func : int -> int -> int) : TiState =
    let argAddrs = getArgs state.Heap (List.take 3 state.Stack)
    match List.map (isDataNode state.Heap) argAddrs with
    | [true; true] ->
        let rootAddr = List.item 2 state.Stack
        let nodes = List.map (getNodeNotInderect state.Heap) argAddrs
        match nodes with
        | [NNum n1; NNum n2] ->
            let newNode = func n1 n2 |> NNum
            let newHeap = heapUpdate state.Heap rootAddr newNode
            let newStack = List.skip 2 state.Stack
            { state with
                Stack = newStack 
                Heap = newHeap }
        | _ ->
            failwith "Two number nodes are expected"
    | [false; _] ->
        evalNodeOnNewStack state (List.item 0 argAddrs)
    | [_; false] ->
        evalNodeOnNewStack state (List.item 1 argAddrs)
    | _ ->
        failwith "wrong arg addresses"

let primStep (state : TiState) = function
    | Neg -> primNeg state
    | Add -> primArith state (+)
    | Sub -> primArith state (-)
    | Div -> primArith state (/)
    | Mul -> primArith state (*)
    
let numStep (state : TiState) n = 
    match state.Stack with
    | [addr] when isDataNode state.Heap addr ->
        { state with
            Stack = List.head state.Dump 
            Dump = List.tail state.Dump }
    | _ -> 
        failwith "Expected in numStep to have stack with only num node on the top"

let apStep (state : TiState) a1 a2 =
    { state with Stack = a1 :: state.Stack }

let scStep (state : TiState) scName argNames body =
    let requiredLength = List.length argNames + 1
    if List.length state.Stack < requiredLength then
        failwith "Stack contains less entries that are required for supercombinator"
    let args = getArgs state.Heap state.Stack
    let minLen = min args.Length argNames.Length
    let argBindings = List.zip (List.take minLen argNames) (List.take minLen args)
    let env = argBindings |>List.append<| state.Globals

    let rootAddr = getRootAddr state.Stack argNames
    let heap1 = instantiateAndUpdate body rootAddr state.Heap env
    let newStack = 
        rootAddr :: (List.skip requiredLength state.Stack)

    { state with
        Stack = newStack
        Heap = heap1 }

let indStep state addr =
    { state with Stack = addr :: (List.tail state.Stack) }

let showFWAddr addr = 
    let str = string addr
    iStr (space (4 - Seq.length str) + str)

let showNode = function
    | NNum n -> iStr "NNum " |>iAppend<| iNum n
    | NAp (a1, a2) -> 
        iConcat [ iStr "NAp "; showAddr a1; iStr " "; showAddr a2 ]
    | NSc (name, args, body) -> iStr ("NSc " + name)
    | NInd addr -> iStr "NInd " |>iAppend<| iNum addr
    | NPrimitive (name, prim) -> 
        iConcat [
            iStr "NPrimitive "; iStr name;
        ]

let showStackNode heap = function
    | NAp (funAddr, argAddr) -> 
        iConcat [ iStr "NAp "; showFWAddr funAddr;
                  iStr " "; showFWAddr argAddr;
                  iStr " ("; heapLookup heap argAddr |> showNode; iStr ")" ]
    | node -> showNode node

let showStack heap stack =
    let showStackItem addr = 
        iConcat [ showFWAddr addr; iStr ": "; 
                  heapLookup heap addr |> showStackNode heap ]
    iConcat [ 
        iStr "Stk [";
        iIndent (iInterleave iNewline (List.map showStackItem stack));
        iStr " ]"
    ]

let showHeap (heap : TiHeap) =
    let showHeapElt (addr, node) =
        iConcat [
            iStr "("; iFWNum 4 addr; iStr ") ";
            showNode node; iNewline
        ]

    Map.toList heap
    |> List.map showHeapElt
    |> iConcat

let showState (state : TiState) =
    iConcat [ 
        showStack state.Heap state.Stack; iNewline;
        showHeap state.Heap; iNewline 
    ]

let step state = 
    match heapLookup state.Heap (List.head state.Stack) with
    | NNum n -> numStep state n
    | NAp (a1, a2) -> apStep state a1 a2
    | NSc (name, args, body) -> scStep state name args body
    | NInd addr -> indStep state addr
    | NPrimitive (name, prim) -> primStep state prim

let rec eval state = 
    let restStates = 
        if tiFinal state then []
        else
            state |> step |> doAdmin |> eval
    state :: restStates

let showStats state =
    iConcat [ iNewline; iNewline; iStr "Total number of steps = "; 
              iNum (tiStatGetSteps state.Stats) ]

let showResults states =
    iConcat [
        iLayn (List.map showState states);
        showStats (List.last states)
    ] |> iDisplay
    
let runProg<'a> = parse >> compile >> eval >> showResults
