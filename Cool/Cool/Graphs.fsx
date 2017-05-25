type Edge<'a> = 'a * 'a
type Graph<'a> = 'a list * Edge<'a> list

type Node<'a> = 'a * 'a list
type AdjacencyGraph<'a> = Node<'a> list

let n : Node<int> = 1,[1..20]
let graph2adjacency ((nodes, edges) : Graph<'a>) =
    let mapa =
        nodes
        |> List.map (fun n -> (n, []))
        |> Map.ofList
    edges
    |> List.fold (fun mapa (src, dst) -> 
        match Map.tryFind src mapa with
        | Some ns -> mapa |> Map.add src (dst::ns)
        | None -> mapa |> Map.add src []) mapa
    |> Map.toList

let adjacency2graph (ns : AdjacencyGraph<'a>) =
    let nodes = ns |> List.map fst
    let edges =
        ns
        |> List.collect (fun (src, dsts) -> 
            dsts |> List.map (fun dst -> src, dst))
    nodes, edges

let findNode curr g = List.tryFind (fst >> (=) curr) g

let path (src:'a) (dst:'a) (g:AdjacencyGraph<'a>) =
    let rec go visited curr : 'a list list =
        if curr = dst then [[curr]]
        else
            match findNode curr g with
            | Some (_, ns) ->
                ns
                |> List.filter (fun x -> Set.contains x visited |> not)
                |> List.collect (go (Set.add curr visited))
                |> List.map (fun x -> curr :: x)
            | None -> []
    go Set.empty src

let rec cutPath path x = 
    match path with
    | y :: ys -> 
        if x = y then [x]
        else y :: (cutPath ys x)
    | [] -> []
    
(**)
let findCycles (start:'a) (g:AdjacencyGraph<'a>) =
    let rec go path curr  =
        let newPath = curr::path
        match findNode curr g with
        | Some (_, ns) ->        
            let visited, unvisited = 
                ns
                |> List.partition (fun x -> List.contains x newPath)
            let cycles1 =
                visited
                |> List.map (cutPath newPath)
            let cycles2 =
                unvisited
                |> List.collect (go newPath)
            List.append cycles1 cycles2
        | None -> []

    go [] start

let toposort g =
    let rec go curr visited =
        let visitedNew = curr :: visited
        match findNode curr g with
        | None -> []
        | Some (_, ns) ->
            let next = 
                ns
                |> List.fold (fun acc x ->
                if List.contains x visitedNew then 
                    acc
                else
                    List.append acc (go x visitedNew)) List.empty
            List.append next [curr]
            
    
    g
    |> List.map fst
    |> List.fold (fun acc x ->
        if List.contains x acc then 
            acc
        else
            List.append acc (go x acc)) List.empty

let edges2adjacency edges =
    edges
    |> List.fold (fun nodes (x, y) ->
        let nodesNew =    
            match Map.tryFind x nodes with
            | Some ns ->
                Map.add x (y::ns) nodes
            | None -> Map.add x [y] nodes
        if Map.containsKey y nodesNew then nodesNew
        else
            Map.add y [] nodesNew) Map.empty
    |> Map.toList

let g = (['b';'c';'d';'f';'g';'h';'k'],[('b','c');('b','f');('c','f');('f','k');('g','h')])
let ggg : AdjacencyGraph<char> =
  [('b', ['f'; 'c']); ('c', ['f']); ('d', []); ('f', ['k']); ('g', ['h']);
   ('h', []); ('k', [])]
let g2 = [  1, [2;5]
            2, [3]
            3, [4]
            5, [4] ]
        
let g3 = [  1, [2]
            2, [3]
            3, [2; 5] ]

let g4 = [  1, [2]
            2, [3;6]
            3, [4]
            4, [2; 5]
            6, [7]
            7, [6] ]
let g5 = [  1, [2]
            2, [9;3]
            3, [4]
            4, [5]
            9, []
            11, [10]
            10, [9] ]
let g6 = [1,[2;4];2,[3];3,[];4,[5;6];5,[];6,[];11,[12;13];12,[6];13,[14];14,[]]
toposort g6

edges2adjacency [1,2;2,3;3,4;1,5;1,4;1,3]