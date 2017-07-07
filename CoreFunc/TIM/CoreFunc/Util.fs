module Util


type Addr = int

type Heap<'a> = { LastAddr : Addr; Map : Map<Addr, 'a> }

let heapAlloc { LastAddr = lastaddr; Map = heap } value =
    let addr = lastaddr + 1
    { LastAddr = addr; Map = Map.add addr value heap }, addr

let heapLookup heap addr = 
    Map.find addr heap.Map
let heapTryLookup heap addr = Map.tryFind addr heap.Map
let heapUpdate heap addr newValue =
    { heap with Map = Map.add addr newValue heap.Map }
let heapRemove heap addr =
    { heap with Map = Map.remove addr heap.Map }
let heapNull : Addr = -1
let heapEmpty = { LastAddr = 0; Map = Map.empty }
let heapAddrsAndElements heap = Map.toList heap.Map
let heapSize heap = Seq.length heap.Map


let mapAccumul (f : 'acc -> 'a -> 'acc * 'b) (initialAcc : 'acc) (xs : 'a list) : 'acc * 'b list =
    xs 
    |> List.fold (fun (acc, ys) elt ->
        let acc', y = f acc elt
        acc', y :: ys) (initialAcc, [])

let listTryFindFirst x = List.tryFind (fst >> ((=) x))