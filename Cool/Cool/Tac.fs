module Tac

open Display
open System.Text.RegularExpressions

type Rhs =
    | Var of string
    | BinOp of string * string * string
    | Not of string
    | Negate of string
    | New of string
    | Default of string
    | Isvoid of string
    | FunCall of string * (string list)
    | IntRhs of int
    | BoolRhs of bool
    | StringRhs of string

type Stmt =
    | Assignment of string * Rhs
    | Jump of string 
    | Label of string
    | Return of string
    | Comment
    | BranchTrue of string * string

let readTac (s : string) : Stmt list =
    let mergeStringLiterals lines =
        lines
        |> List.fold (fun (acc : string list) (line : string) ->
            match acc with
            | prev :: rest when Regex.IsMatch(prev, ".*<-.*string") ->
                (prev + (" ") + line) :: rest
            | _ -> line :: acc) []

    let isNotEmpty (s:string) = s.Length > 0
    let processLine (line : string) =
        let words = line.Split(' ') |> List.ofArray |> List.filter (isNotEmpty)
        match words with
        | ["jmp"; label] -> Jump label
        | ["label"; label] -> Label label
        | ["return"; var] -> Return var
        | ["bt"; var; label] -> BranchTrue (var, label)
        | [var; "<-"; "call"; f; arg] -> Assignment (var, FunCall (f, [arg]))
        | [var; "<-"; op; v1; v2] -> Assignment (var, BinOp (op, v1, v2))
        | [var; "<-"; "int"; n] -> Assignment (var, IntRhs (int n))
        | [var; "<-"; "bool"; n] -> Assignment (var, BoolRhs (System.Boolean.Parse n))
        | [var; "<-"; "string"; s] -> Assignment (var, StringRhs s)
        | [var; "<-"; "call"; f] -> Assignment (var, FunCall (f, []))
        | [var; "<-"; "not"; x] -> Assignment (var, Not x)
        | [var; "<-"; "~"; x] -> Assignment (var, Negate x)
        | [var; "<-"; "new"; x] -> Assignment (var, New x)
        | [var; "<-"; "default"; x] -> Assignment (var, Default x)
        | [var; "<-"; "isvoid"; x] -> Assignment (var, Isvoid x)
        | [var; "<-"; varRhs] -> Assignment (var, Var varRhs)
        | head :: _ when head.StartsWith("#") || head.StartsWith(";") -> 
            Comment
        | _ -> Comment
        | w -> failwithf "read: unknown %A" w
        
    s.Split('\n')
    |> List.ofArray
    |> mergeStringLiterals
    |> List.filter isNotEmpty
    |> List.map processLine
    |> List.rev

let showRhs = function
    | Var s -> iStr s
    | FunCall (f, [args]) -> iSpace [iStr "call"; iStr f; iStr args]
    | BinOp (op, v1, v2) -> iSpace [iStr op; iStr v1; iStr v2]
    | IntRhs n -> iSpace [iStr "int"; iNum n]
    | BoolRhs b -> iSpace [iStr "bool"; b.ToString() |> iStr]
    | StringRhs s -> iSpace [iStr "string"; iStr s]
    | FunCall (f, []) -> iSpace [iStr "call"; iStr f]
    | Not v -> iSpace [iStr "not"; iStr v]
    | New v -> iSpace [iStr "new"; iStr v]
    | Default v -> iSpace [iStr "default"; iStr v]
    | Isvoid v -> iSpace [iStr "isvoid"; iStr v]
    | _ -> failwith "showRhs: case not implemented"

let showStmt = function
    | Assignment (v, rhs) ->
        iSpace [iStr v; iStr "<-"; showRhs rhs]
    | Jump lab -> iSpace [iStr "jmp"; iStr lab]
    | Label lab -> iSpace [iStr "label"; iStr lab]
    | Return v -> iSpace [iStr "return"; iStr v]
    | BranchTrue (v, lab) -> iSpace [iStr "bt"; iStr v; iStr lab]
    | _ -> INil

let showStmts bs = List.map showStmt bs |> iInterleave iNewline
let showBlockss bss = List.map showStmts bss |> iInterleave (iConcat [iNewline; iNewline])

type Prog = Stmt list
type Block = Stmt list
type Vars = Set<string>

let stmtsToBlocks tac =
    let reverses blocks =
        List.map List.rev blocks |> List.rev
    let rec loop block blocks = function
        | t :: tac ->
            match t with
            | Jump _ | BranchTrue _ | Return _ -> 
                loop [] ((t::block)::blocks) tac
            | Label _ when block <> []-> loop [t] (block::blocks) tac
            | Label _ -> loop [t] (blocks) tac
            | Comment -> loop block blocks tac
            | _ -> loop (t::block) blocks tac
        | _ when block <> [] -> reverses (block::blocks)
        | _ -> reverses blocks
    
    loop [] [] tac
    
type Node = {
    Id : int
    mutable Stmts : Stmt list
    mutable Outgoing : Node list
    mutable Incoming : Node list
}

type Graph = {
    mutable Start : Node
    mutable End : Node
    mutable Nodes : Node list
}

let nodeLabel node =
    match List.tryPick (function | Label lab -> Some lab | _ -> None) node.Stmts with
    | Some lab -> lab
    | None -> node.Stmts|> sprintf "%A"

let addEdge nodeFrom (nodeTo:Node) =
    if List.exists (fun node -> node.Id = nodeTo.Id) nodeFrom.Outgoing |> not then
        printfn "addEdge: from<%s> to<%s>" (nodeLabel nodeFrom) (nodeLabel nodeTo)
        nodeFrom.Outgoing <- nodeTo :: nodeFrom.Outgoing
        nodeTo.Incoming <- nodeFrom :: nodeTo.Incoming

let addStmt node stmt =
    node.Stmts <- stmt :: node.Stmts

let createNode id = {
    Id = id
    Stmts = []
    Outgoing = []
    Incoming = []
}

let freshId =
    let mutable n = 0
    fun () ->
        n <- n + 1
        n

let blocksToGraph blocks =
    let nodes =
        List.map (fun b -> 
            let n = createNode(freshId())
            n.Stmts <- b
            n) blocks
    
    let g = {
        Start = List.head nodes
        End = List.find (fun b -> List.exists (function | Return _ -> true | _ -> false) b.Stmts) nodes
        Nodes = nodes
    }

    let labeled =
        List.fold (fun labeled n ->
            match n.Stmts with
            | Label lab :: _ -> Map.add lab n labeled
            | _ -> labeled) Map.empty g.Nodes
    
    let addEdgeToNext curr = function
        | next :: _ -> addEdge curr next
        | _ -> ()

    let rec buildEdges = function
        | node :: restNodes ->
            match List.last node.Stmts with
            | Jump lab ->
                printfn "Jump: %s %A" lab labeled
                let dst = Map.find lab labeled
                addEdge node dst
            | BranchTrue (_, lab) ->
                printfn "Branch: %s %A" lab labeled
                let dst = Map.find lab labeled
                addEdgeToNext node restNodes
                addEdge node dst
            | _ -> 
                addEdgeToNext node restNodes
            buildEdges restNodes
         | _ -> ()

    buildEdges g.Nodes
    g

let graphToStmts graph =
    let rec loop todo doneIds blocks =
        
        match Queue.tryDequeue todo with
        | None -> List.rev blocks
        | Some (node, restTodo) when Set.contains node.Id doneIds ->
            printfn "graphToStmts: loop: node <%s>" (nodeLabel node)
            loop restTodo doneIds blocks
        | Some (node, restTodo) -> 
            printfn "graphToStmts: loop: node <%s>" (nodeLabel node)
            loop (Queue.enqueueList node.Outgoing restTodo) (Set.add node.Id doneIds) (node.Stmts :: blocks)
    
    loop (Queue.singleton graph.Start) Set.empty []
    |> List.collect (fun x -> x)
    
let getUsedInRhs rhs = 
    match rhs with
    | BinOp (_, v1, v2) -> [v1; v2]
    | Var v -> [v]
    | Rhs.Isvoid v -> [v]
    | Rhs.FunCall (_, vs) -> vs
    | Not v -> [v]
    | Negate v -> [v]
    | _ -> []

    |> Set.ofList


let eliminateUnusedAndGetUsedDefd (block:Block) (used:Vars) : Block * Vars * Vars =

    let shouldEliminateAssignment v rhs used defd =
        match rhs with
        | Rhs.FunCall ("out_int", _) | Rhs.FunCall ("in_int", _) -> false
        | _ -> Set.contains v used |> not || Set.contains v defd
        
    let rec loop (block, used, defd as acc) stmt =
        match stmt with
        | Assignment (v, rhs) when shouldEliminateAssignment v rhs used defd ->
            acc
        | Assignment (v, rhs) ->
            stmt::block, (getUsedInRhs rhs |> Set.union used), Set.add v defd
        | Jump _ | Label _ -> stmt::block, used, defd
        | Return v | BranchTrue (v, _) -> stmt::block, used.Add v, defd
        | _ -> stmt::block, used, defd

    List.fold loop ([], used, Set.empty) (List.rev block)

let getUsedAndDefined node =
    let block = node.Stmts
    List.fold (fun (used, defined as acc) stmt ->
        match stmt with
        | Assignment (v, rhs) ->
            let used = getUsedInRhs rhs |> Set.add v |> Set.union used
            let defined = Set.add v defined
            used, defined
        | Return v | BranchTrue (v, _) ->
            Set.add v used, defined
        | _ -> acc) (Set.empty, Set.empty) block

let processNode2 node used defined liveoutMap =
    let liveoutOld = Map.find node.Id liveoutMap
    let liveout = Set.union (Set.difference liveoutOld defined) used
    let loop (todo, liveoutMap) pred = 
        let liveoutPred = Map.find pred.Id liveoutMap
        if Set.isSubset liveout liveoutPred then
            todo, liveoutMap
        else
            let liveoutPred = Set.union liveout liveoutPred
            let liveoutMap = Map.add pred.Id liveoutPred liveoutMap
            pred :: todo, liveoutMap
    
    List.fold loop ([], liveoutMap) node.Incoming

let getLiveout graph =
    let udMap = 
        List.map (fun node -> node.Id, getUsedAndDefined node) graph.Nodes 
        |> Map.ofList
    
    let liveoutMapInit =
        List.map (fun node -> node.Id, Set.empty) graph.Nodes
        |> Map.ofList

    let rec loop todo liveoutMap =
        match todo with
        | [] -> liveoutMap
        | node :: restTodo ->
            let used, defined = Map.find node.Id udMap
            let newTodo, liveoutMap = processNode2 node used defined liveoutMap
            loop (List.append newTodo todo) liveoutMap
    
    loop [graph.End] liveoutMapInit

let eliminateInNode node liveout =
    let shouldLive = function
        | Assignment (_, Rhs.FunCall ("out_int", _)) | Assignment (_, Rhs.FunCall ("in_int", _)) -> 
            true
        | Assignment (v, rhs) -> Set.contains v liveout
        | _ -> true
    let stmts = List.filter shouldLive node.Stmts
    let changed = node.Stmts.Length <> stmts.Length
    node.Stmts <- stmts
    changed

let eliminateInGraph2 graph =
    let eliminateInNodes liveoutMap =
        List.fold (fun changed node ->
            let liveout = Map.find node.Id liveoutMap
            let changed' = eliminateInNode node liveout
            changed' || changed) false graph.Nodes

    let rec loop = function
        | true -> 
            let liveoutMap = getLiveout graph
            eliminateInNodes liveoutMap
            |> loop
        | false -> ()

    loop true

let processNode usedMap node =
    let usedAfter = Map.find node.Id usedMap
    let stmts, usedInNode, defd = eliminateUnusedAndGetUsedDefd node.Stmts usedAfter
    let usedBefore = Set.union (Set.difference usedAfter defd) usedInNode
    
    node.Stmts <- stmts
    printfn "processNode: label<%s> id<%d> usedAfter<%A> usedInNode<%A> usedBefore<%A>" (nodeLabel node) node.Id usedAfter usedInNode usedBefore

    List.fold (fun (todo, usedMap) pred ->
        printfn "processNode:    pred label<%s> id<%d>" (nodeLabel pred) pred.Id
        let usedByPred = Map.find pred.Id usedMap
        if Set.isSubset usedBefore usedByPred then
            todo, usedMap
        else
            pred :: todo, Map.add pred.Id (Set.union usedBefore usedByPred) usedMap) 
        ([], usedMap) 
        node.Incoming

let eliminateInGraph (graph:Graph) =
    let isNodesSame node1 node2 = node1.Id = node2.Id

    let rec loop usedMap todoQ =
        match Queue.tryDequeue todoQ with
        | None -> ()
        | Some (node, restTodo) ->
            let todoList, usedMap = processNode usedMap node
            loop usedMap (Queue.enqueueListUnique isNodesSame todoList restTodo)
    let usedMap =
        List.fold (fun usedMap node ->
            Map.add node.Id Set.empty usedMap) Map.empty graph.Nodes
    
    loop usedMap (Queue.singleton graph.End)

let eliminateDeadInStmts stmts =
    let graph = stmts |> stmtsToBlocks |> blocksToGraph
    eliminateInGraph graph
    graphToStmts graph