module Compile 

open Language
open Util

type TiStack = Addr list
type TiDump = TiStack list

type Primitive =
    | Neg
    | Add
    | Sub
    | Mul
    | Div
    | If
    | Greater
    | GreaterEq
    | Less
    | LessEq
    | Eq
    | NotEq
    | PConstr of int * int

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

let extraPreludeDefs = [
    "True", [], EConstr (2, 0)
    "False", [], EConstr (1, 0)
    "and", ["x"; "y"], EAp (EAp (EAp (EVar "if", EVar "x"), EVar "y"), EVar "False")
    "or", ["x"; "y"], EAp (EAp (EAp (EVar "if", EVar "x"), EVar "True"), EVar "y")
    "not", ["x"], EAp (EAp (EAp (EVar "if", EVar "x"), EVar "False"), EVar "True")
    "xor", ["x"; "y"], 
        EAp 
            (EAp 
                (EAp 
                    (EVar "if", EVar "x"), 
                    EAp (EVar "not", EVar "y")), 
            EVar "y")
]

let primitives = [ 
    "negate", Neg
    "+", Add
    "-", Sub
    "*", Mul 
    "/", Div
    ">", Greater
    ">=", GreaterEq
    "<", Less
    "<=", LessEq
    "==", Eq
    "~=", NotEq
    "if", If
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
    | NData _ -> true
    | NInd addr2 -> isDataNode heap addr2
    | _ -> false

let isDataNodeSimple = function
    | NNum n -> true
    | NData _ -> true
    | _ -> false


let tiFinal = function
    | { Stack = [soleAddr]; Heap = heap; Dump = [] } ->
        isDataNode heap soleAddr
    | { Stack = [] } -> 
        failwith "Empty stack!"
    | _ -> 
        false

let instantiateConstr tag arity heap env =
    heapAlloc heap (NPrimitive ("PConstr", (PConstr (tag, arity))))
let instantiateAndUpdateConstr tag arity updAddr heap env =
    heapUpdate heap updAddr (NPrimitive ("PConstr", (PConstr (tag, arity))))

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
        | EConstr (tag, arity) ->
            instantiateConstr tag arity heap env
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
    | EConstr (t, a) ->
        instantiateAndUpdateConstr t a updAddr heap env
    | _ ->
        failwith "Con't instantiate lambda expression, case or constructor"

let getArgs heap stack =
    let rec getArg addr = 
        match heapLookup heap addr with
        | NAp (func, arg) -> arg
        | NInd addr -> getArg addr
        | n -> failwithf "NAp node is expected, but got [%d ; %A] stack %A"  addr n stack
    List.map getArg (List.tail stack)

let getArg heap a =
    match heapLookup heap a with
    | NAp (_, a1) -> a1
    | _ ->
        failwith "Application node is expected"


let getRootAddr stack argNames =
    List.item (List.length argNames) stack 

let rec getNodeNotInderect heap addr =
    match heapLookup heap addr with
    | NInd addr' -> getNodeNotInderect heap addr'
    | n -> n

let evalNodeOnNewStack state nodeAddr =
    { state with
        Stack = [nodeAddr] 
        Dump = (List.tail state.Stack) :: state.Dump }

let primNeg (state : TiState) =
    assert (state.Stack.Length = 2) // a :: a1 :: []

    let argAddr = getArgs state.Heap (List.take 2 state.Stack) |> List.head
    match heapLookup state.Heap argAddr with
    | NNum n -> 
        let rootAddr = List.item 1 state.Stack
        let newHeap = heapUpdate state.Heap rootAddr (NNum -n)
        let newStack = List.tail state.Stack
        { state with
            Stack = newStack
            Heap = newHeap }
    | _ ->
        evalNodeOnNewStack state argAddr

let primDyadic (state : TiState) (func : Node -> Node -> Node) : TiState =
    let lookup (x, y) = (heapLookup state.Heap x, heapLookup state.Heap y)
    match state.Stack with
    | a :: a1 :: [a2] ->
        match lookup (a1, a2) with
        | NAp (_, b1), NAp (_, b2) ->
            match lookup (b1, b2) with
            | n1, n2 when isDataNodeSimple n1 && isDataNodeSimple n2->
                let n3 = func n1 n2
                let newHeap = heapUpdate state.Heap a2 n3
                { state with 
                    Stack = [a2] 
                    Heap = newHeap }
            | n1, _ when isDataNodeSimple n1 ->
                { state with
                    Stack = [b2]
                    Dump = [a2] :: state.Dump }
            | _, _ ->
                { state with
                    Stack = [b1]
                    Dump = [a2] :: state.Dump }
        | _ ->
            failwith "Two application are expected in stack for primitive ops dyadic"
    | _ ->
        failwith "wrong arguments to primitive dyadic"

let primArith (state : TiState) (func : int -> int -> int) : TiState =
    let func2 n1 n2 = 
        match n1, n2 with
        | NNum n1, NNum n2 -> func n1 n2 |> NNum
        | _ -> failwith "two number nodes are expected for arithmetic op"
    primDyadic state func2

let boxBool = function
    | true -> NData (2, [])
    | false -> NData (1, [])

let unboxBool = function
    | NData (2, []) -> true 
    |  NData (1, []) -> false
    | _ -> failwith "boolx expected"

let isBool = function
    | NData (2, []) |  NData (1, []) -> true
    | _ -> false

let primComp (state : TiState) (func : int -> int -> bool) : TiState =
    let func2 n1 n2 = 
        match n1, n2 with
        | NNum n1, NNum n2 -> 
            func n1 n2 |> boxBool
        | _ -> failwith "two number nodes are expected for arithmetic op"
    primDyadic state func2

let primEq (state : TiState) (eq : bool) : TiState =
    let negate x = if eq then x else not x
    let func2 n1 n2 = 
        match n1, n2 with
        | NNum n1, NNum n2 -> 
            n1 = n2 |> negate |> boxBool 
        | n1, n2 when isBool n1 && isBool n2 ->
            (unboxBool n1) = (unboxBool n2) |> negate |> boxBool
        | _ -> failwith "two number nodes are expected for arithmetic op"
    primDyadic state func2


let primConstr state t n =
    let bs = getArgs state.Heap state.Stack
    let newStack = List.skip n state.Stack
    let an = List.head newStack
    let newHeap = heapUpdate state.Heap an (NData (t, bs))
    { state with 
        Stack = newStack
        Heap = newHeap }

let ifStep state =
    match state.Stack with
    | [a0; a1; a2; a3] ->
        let b = getArg state.Heap a1
        match heapLookup state.Heap b with
        | NData (2, []) ->
            let addrThen = getArg state.Heap a2
            { state with
                Stack = [addrThen] }
        | NData (1, []) ->
            let addrElse = getArg state.Heap a3
            { state with
                Stack = [addrElse] }
        | _ ->
            { state with
                Stack = [b]
                Dump = [a3] :: state.Dump }
    | _ ->
        failwith "Wrong arguments for If"

let primStep (state : TiState) = function
    | Neg -> primNeg state
    | Add -> primArith state (+)
    | Sub -> primArith state (-)
    | Div -> primArith state (/)
    | Mul -> primArith state (*)
    | Greater -> primComp state (>)  
    | GreaterEq -> primComp state (>=)
    | Less -> primComp state (<)
    | LessEq -> primComp state (<=)
    | Eq -> primEq state true
    | NotEq -> primEq state false
    | PConstr (t, a) -> primConstr state t a
    | If -> ifStep state
    | _ -> failwith "wrong function primitive"
    
let apStep (state : TiState) a a1 a2 =
    match heapLookup state.Heap a2 with
    | NInd a3 ->
        printfn "replace inderaction node to %d in application" a3
        { state with Heap = heapUpdate state.Heap a (NAp (a1, a3)) }
    | _ ->        
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
    | NData (2, []) -> iStr "True"
    | NData (1, []) -> iStr "False"
    | NData (tag, addrs) -> 
        iConcat [ 
            iStr "NData"; iNum tag; iStr " len"; iNum (List.length addrs) 
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

let showDump (state : TiState) =
    iConcat [
        iStr "Dump: [[";
        List.map (fun x -> List.take (min 3 (List.length x)) x |> (showStack state.Heap)) state.Dump |> iInterleave iNewline |> iIndent;
        iStr "]]"
    ]

let showState (state : TiState) =
    iConcat [ 
        showStack state.Heap state.Stack; iNewline;
        showDump state; iNewline;
        showHeap state.Heap; iNewline 
    ]

let numStep (state : TiState) n = 
    match state.Stack, state.Dump with
    | [addr], (stack :: stacks) when isDataNode state.Heap addr ->
        { state with
            Stack = stack
            Dump = stacks }
    | _ -> 
        failwith "Expected in numStep to have stack with only num node on the top"

let dataStep state t a =
    match state.Stack, state.Dump with
    | [addr], (stack :: stacks) when isDataNode state.Heap addr ->
        { state with
            Stack = stack
            Dump = stacks }
    | _ -> 
        failwith "Expected in dataStep to have stack with only data node on the top"

let step state = 
    let a = (List.head state.Stack)
    match heapLookup state.Heap a with
    | NNum n -> numStep state n
    | NAp (a1, a2) -> apStep state a a1 a2
    | NSc (name, args, body) -> scStep state name args body
    | NInd addr -> indStep state addr
    | NPrimitive (name, prim) -> primStep state prim
    | NData (t, a) -> dataStep state t a

let rec eval state = 
    printfn "%s %d" (showState state |> iDisplay) state.Stack.Length
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
