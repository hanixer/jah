module Compile 

open Language

type Addr = int

type TiStack = Addr list
type TiDump = DummyDump

type Node =
    | NAp of Addr * Addr
    | NSc of Name * Name list * CoreExpr
    | NNum of int

type Heap<'a> = Map<Addr, 'a>
type TiHeap = Heap<Node>

type TiGlobals = (Name * Addr) list

type TiStats = int

type TiState =  
    { Stack : TiStack 
      Dump : TiDump
      Heap : TiHeap 
      Globals : TiGlobals
      Stats : TiStats }

let initialTiDump = DummyDump

let tiStatInitial = 0
let tiStatIncSteps s = s + 1
let tiStatGetSteps s = s
let applyToStats func state =
    { state with Stats = func state.Stats}

let extraPreludeDefs = []

let heapAlloc heap value =
    let addr = (heap |> Map.toSeq |> Seq.map fst |> Seq.append [0] |> Seq.max) + 1
    Map.add addr value heap, addr    

let heapLookup heap addr = 
    Map.find addr heap
let heapTryLookup heap addr = Map.tryFind addr heap
let heapUpdate heap addr newValue =
    Map.add addr newValue heap
let heapRemove heap addr =
    Map.remove addr heap

let allocateSc heap (name, args, body) =
    let heap', addr = heapAlloc heap (NSc (name, args, body))
    heap', (name, addr)

let mapAccumul (f : 'acc -> 'a -> 'acc * 'b) (initialAcc : 'acc) (xs : 'a list) : 'acc * 'b list =
    xs 
    |> List.fold (fun (acc, ys) elt ->
        let acc', y = f acc elt
        acc', y :: ys) (initialAcc, [])

let buildInitialHeap scDefns = 
    mapAccumul allocateSc Map.empty scDefns

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

let isDataNode node =
    match node with
    | NNum n -> true
    | _ -> false

let tiFinal = function
    | { Stack = [soleAddr]; Heap = heap } ->
        heapLookup heap soleAddr |> isDataNode
    | { Stack = [] } -> 
        failwith "Empty stack!"
    | _ -> 
        false

let instantiateConstr tag arity heap env =
    failwith "Can't instantiate constr now"

let showHeap2 (heap : TiHeap) =
    let showAddr = string >> iStr
    let showNode = function
        | NNum n -> iStr "NNum " |>iAppend<| iNum n
        | NAp (a1, a2) -> 
            iConcat [ iStr "NAp "; showAddr a1; iStr " "; showAddr a2 ]
        | NSc (name, args, body) -> iStr ("NSc " + name)
    let showHeapElt (addr, node) =
        iConcat [
            iStr "("; iFWNum 4 addr; iStr ") ";
            showNode node; iNewline
        ]

    Map.toList heap
    |> List.map showHeapElt
    |> iConcat
    

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

    
let numStep (state : TiState) n = failwith "Number applied as a function!"

let apStep (state : TiState) a1 a2 =
    { state with Stack = a1 :: state.Stack }

let getArgs heap stack =
    let getArg addr = 
        match heapLookup heap addr with
        | NAp (func, arg) -> arg
        | _ -> failwith "NAp node is expected"
    List.map getArg (List.tail stack)

let scStep (state : TiState) scName argNames body =
    let requiredLength = List.length argNames + 1
    if List.length state.Stack < requiredLength then
        failwith "Stack contains less entries that are required for supercombinator"
    let args = getArgs state.Heap state.Stack
    let minLen = min args.Length argNames.Length
    let argBindings = List.zip (List.take minLen argNames) (List.take minLen args)
    let env = argBindings |>List.append<| state.Globals
    let newHeap, resultAddr = instantiate body state.Heap env
    let newStack = 
        resultAddr :: (List.skip requiredLength state.Stack)

    { state with
        Stack = newStack
        Heap = newHeap }

let step state = 
    match heapLookup state.Heap (List.head state.Stack) with
    | NNum n -> numStep state n
    | NAp (a1, a2) -> apStep state a1 a2
    | NSc (name, args, body) -> scStep state name args body

let showFWAddr addr = 
    let str = string addr
    iStr (space (4 - Seq.length str) + str)

let showAddr = string >> iStr

let showNode = function
    | NNum n -> iStr "NNum " |>iAppend<| iNum n
    | NAp (a1, a2) -> 
        iConcat [ iStr "NAp "; showAddr a1; iStr " "; showAddr a2 ]
    | NSc (name, args, body) -> iStr ("NSc " + name)

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
