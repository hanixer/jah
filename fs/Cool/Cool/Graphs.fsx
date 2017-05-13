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

let path (src:'a) (dst:'a) (g:AdjacencyGraph<'a>) =
    let rec go curr : 'a list list =
        if curr = dst then [[curr]]
        else
            match List.tryFind (fst >> (=) curr) g with
            | Some (_, ns) ->
                ns
                |> List.collect go
                |> List.map (fun x -> curr :: x)
            | None -> []
    go src

let g = (['b';'c';'d';'f';'g';'h';'k'],[('b','c');('b','f');('c','f');('f','k');('g','h')])
let ggg : AdjacencyGraph<char> =
  [('b', ['f'; 'c']); ('c', ['f']); ('d', []); ('f', ['k']); ('g', ['h']);
   ('h', []); ('k', [])]
let g2 = [  1, [2;5]
            2, [3]
            3, [4]
            5, [4] ]
path 1 4 g2