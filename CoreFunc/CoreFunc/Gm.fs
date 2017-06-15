module Gm

open Language
open Util

type GmGlobals = (Name * Addr) list

type Instruction =
    | Unwind
    | Pushglobal of Name
    | Pushint of int
    | Push of int
    | Mkap
    | Slide of int

type GmCode = Instruction list

type GmStack = Addr list

type Node =
    | NAp of Addr * Addr
    | NNum of int
    | NGlobal of int * GmCode

type GmHeap = Heap<Node>

type GmStats = int

type GmState =
    { Code : GmCode
      Heap : GmHeap
      Globals : GmGlobals
      Stack : GmStack
      Stats : GmStats }

type CompiledSC = Name * int * GmCode

let statInitial = 0
let statIncSteps s = s + 1
let statGetSteps s = s

let allocateSc heap (name, nargs, is) =
    let heap', addr = heapAlloc heap (NGlobal (nargs, is))
    heap', (name, addr)

let initialCode = [ Pushglobal "main"; Unwind ]

let buildInitialHeap scDefns = 
    mapAccumul allocateSc Map.empty scDefns


let compile program =
    failwith "unimplemented"    


let gmFinal (s : GmState) =
    s.Code.IsEmpty

let pushglobal f (s : GmState) =
    match List.tryFind (fst >> ((=) f)) s.Globals with
    | Some (_, x) ->
        { s with Stack = x :: s.Stack }
    | _ ->
        failwithf "cannot find global %s" f

let pushint (n : int) (s : GmState) =
    let newHeap, a = heapAlloc s.Heap (NNum n)
    let newStack = a :: s.Stack
    { s with
        Heap = newHeap
        Stack = newStack }

let mkap (s : GmState) =
    match s.Stack with
    | a1 :: a2 :: addrs ->
        let newHeap, a = heapAlloc s.Heap (NAp (a1, a2))
        let newStack =  a :: addrs
        { s with 
            Stack = newStack
            Heap = newHeap }
    | _ -> failwith "Two arguments expected in stack!"

let getArg = function
    | NAp (_, a) -> a
    | _ -> failwith "application node is expected"

let push n s =
    let argAddr = s.Stack |> List.item (n + 1) |> heapLookup s.Heap |> getArg
    { s with Stack = argAddr :: s.Stack }

let slide n s =
    let a = List.head s.Stack
    let rest = List.skip (n + 1) s.Stack
    { s with Stack = a :: rest }

let unwind s =
    assert (s.Code.Length = 1)

    match heapLookup s.Heap (List.head s.Stack)  with
    | NNum n ->
        { s with Code = [] }
    | NAp (a1, a2) -> 
        { s with
            Stack = a1 :: s.Stack 
            Code = [Unwind] }
    | NGlobal (n, c) ->
        if s.Stack.Length < n then
            failwith "Unwinding with two few arguments"
        else
            { s with Code = c }

let dispatch (i : Instruction) =
    match i with
    | Pushglobal f -> pushglobal f
    | Pushint n -> pushint n
    | Mkap -> mkap
    | Push n -> push n
    | Slide n -> slide n
    | Unwind -> unwind
    | _ -> failwith "unimpl"

let step (s : GmState) = 
    match s.Code with
    | i :: is -> dispatch i { s with Code = is }
    | _ -> failwith "expected nonempty code in step"

let doAdmin (s : GmState) =
    { s with 
        Stats = statIncSteps s.Stats}

let rec eval state =
    let restStates =
        if gmFinal state then
            []
        else
            state |> step |> doAdmin |> eval 
    state :: restStates

let showResults states =
    failwith "unimplemented"

let runProg<'p> = parse >> compile >> eval >> showResults