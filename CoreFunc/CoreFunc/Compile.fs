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

type TiGlobals = Map<Name, Addr>

type TiStats = int

type TiState = TiStack * TiDump * TiHeap * TiGlobals * TiStats

let initialTiDump = DummyDump

let tiStatInitial = 0
let tiStatIncSteps s = s + 1
let tiStatGetSteps s = s
let applyToStats func (stack, dump, heap, globals, stats) =
    (stack, dump, heap, globals, func stats)

let extraPreludeDefs = []

let heapAlloc heap value =
    let addr = (heap |> Map.toSeq |> Seq.map fst |> Seq.append [0] |> Seq.max) + 1
    Map.add addr value heap, addr    

let heapLookup heap addr = Map.find addr heap

let initialHeap = Map.empty

let allocateSc heap (name, args, body) =
    let heap', addr = heapAlloc heap (NSc (name, args, body))
    heap', (name, addr)

let mapAccumul (f : 'acc -> 'a -> 'acc * 'b) (initialAcc : 'acc) (xs : 'a list) : 'acc * 'b list =
    xs 
    |> List.fold (fun (acc, ys) elt ->
        let acc', y = f acc elt
        acc', y :: ys) (initialAcc, [])

let buildInitialHeap scDefns = 
    mapAccumul allocateSc initialHeap scDefns


let compile program = 
    let scDefs = program |>List.append<| preludeDefs |>List.append<| extraPreludeDefs
    let initalHeap, globals = buildInitialHeap scDefs
    let addressOfMain = List.tryFind (fst >> ((=) "main")) globals |> defaultArg <| failwith "main is not defined"
    let initialStack = [addressOfMain]

    initialStack, initialTiDump, initialHeap, globals, tiStatInitial

let doAdmin state = applyToStats tiStatIncSteps state

let isDataNode = function
    | NNum n -> true
    | _ -> false

let tiFinal = function
    | [soleAddr], dump, heap, globals, state ->
        heapLookup heap soleAddr |> isDataNode
    | [], _, _, _, _ -> failwith "Empty stack!"
    | _ -> false

let numStep (state : TiState) n = failwith "Number applied as a function!"

let apStep ((stack, dump, heap, globals, stats) : TiState) a1 a2 =
    (a1 :: stack, dump, heap, globals, stats)

let step = id

let rec eval state = 
    let nextState = step state |> doAdmin
    let restStates = 
        if tiFinal state then []
        else
            eval nextState
    state :: restStates

let showResults = id
(*
let runProg<'a> = parse >> compile >> eval >> showResults
*)