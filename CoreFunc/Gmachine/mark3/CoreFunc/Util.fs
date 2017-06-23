module Util


type Addr = int

type Heap<'a> = Map<Addr, 'a>


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
let heapNull : Addr = -1


let mapAccumul (f : 'acc -> 'a -> 'acc * 'b) (initialAcc : 'acc) (xs : 'a list) : 'acc * 'b list =
    xs 
    |> List.fold (fun (acc, ys) elt ->
        let acc', y = f acc elt
        acc', y :: ys) (initialAcc, [])

