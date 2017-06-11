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

let compile program = 
    let scDefs = program |>List.append<| preludeDefs |>List.append<| extraPreludeDefs
    let (initalHeap, globals) = buildInitialHeap scDefs
    let addressOfMain = Map.tryFind "main" globals |> defaultArg <| failwith "main is not defined"
    let initalStack = [addressOfMain]

    initialStack, initialTiDump, initialHeap, globals, tiStatInitial



let eval = id
let showResults = id

let runProg<'a> = parse >> compile >> eval >> showResults