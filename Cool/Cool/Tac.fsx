#load "Display.fsx"
#load "Queue.fs"

open Display
open System.Text.RegularExpressions
open System.Linq.Expressions
open System.Linq.Expressions
open System.Diagnostics

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

let addEdge nodeFrom nodeTo =
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
                let dst = Map.find lab labeled
                addEdge node dst
            | BranchTrue (_, lab) ->
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
            loop restTodo doneIds blocks
        | Some (node, restTodo) -> 
            loop (Queue.enqueueList node.Outgoing restTodo) (Set.add node.Id doneIds) (node.Stmts :: blocks)
    
    loop (Queue.singleton graph.Start) Set.empty []
    |> List.collect (fun x -> x)

let eliminateUnusedAndGetUsedDefd (block:Block) (used:Vars) : Block * Vars * Vars =
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

let processNode usedMap node =
    let usedAfter = Map.find node.Id usedMap
    let stmts, usedInNode, defd = eliminateUnusedAndGetUsedDefd node.Stmts usedAfter
    let usedBefore = Set.union (Set.difference usedAfter defd) usedInNode
    
    node.Stmts <- stmts

    List.fold (fun (todo, usedMap) pred ->
        let usedByPred = Map.find pred.Id usedMap
        if Set.isProperSubset usedBefore usedByPred then
            todo, usedMap
        else
            pred :: todo, Map.add pred.Id (Set.union usedBefore usedByPred) usedMap) 
        ([], usedMap) 
        node.Incoming

let eliminateInGraph (graph:Graph) =
    let isNodesSame node1 node2 = node1.Id = node2.Id

    let rec loop usedMap todoQ =
        printfn "loop: todoQ<%A> usedMap<%A>" todoQ usedMap
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

let p = "
x <- call in_int
z <- int 0
b <- < x z 
bt b is_negative
output <- x
jmp do_the_printing
label is_negative
output <- - z x 
label do_the_printing
retval <- call out_int output 
return retval"

let p2 = "
x <- a
y <- b
x <- e
bt i nothing"

let p3 = "
label new
a <- b
label mew
b <- a
return b"

let p4 = "
bt b is_negative
output <- x
jmp do_the_printing
label is_negative
output <- - z x 
label do_the_printing
retval <- call out_int output 
return retval"

let p5 = "
a <- int 33
bt b tilend
y <- b
label tilend
x <- + y a
return x"

let processStr s =
    readTac s |> eliminateDeadInStmts |> showStmts |> iDisplay

let asrt n (input:string) (expected:string) =
    let input = input.Trim()
    let expected = expected.Trim()
    try
        let p = (processStr input).Trim()
        if p <> expected then
            printfn "FAILED #%d! \nExpected [\n%s] \n\nbut got [\n%s]\n\n" n expected p
    with exc ->
        printfn "FAILED #%d! Exception [%s] in [%s]" n exc.Message input
    

let testcases() =
    asrt 1 p5 p5
    asrt 2 "
a <- int 33
bt b tilend
y <- b
label tilend
x <- + y b
return x" 
        "
bt b tilend
y <- b
label tilend
x <- + y b
return x"

    asrt 3 "
a <- call in_int
bt b tilend
y <- b
label tilend
x <- + y b
return x"
        "
a <- call in_int
bt b tilend
y <- b
label tilend
x <- + y b
return x"

    asrt 4 "
i <- int 555
a <- call in_int
bt b tilend
y <- b
label tilend
x <- i
x <- + y b
return x"
        "
a <- call in_int
bt b tilend
y <- b
label tilend
x <- + y b
return x"

    asrt 5 "
a <- int 1
b <- int 2
bt cond use_b
x <- a
jmp end
label use_b
x <- b
label end
return x"
        "
a <- int 1
b <- int 2
bt cond use_b
x <- a
jmp end
label use_b
x <- b
label end
return x"
    
    asrt 777 "
comment start
label CellularAutomaton_evolve_0
t$1 <- default Int
t$2 <- call num_cells
t$3 <- default String
jmp CellularAutomaton_evolve_3
comment while-pred
label CellularAutomaton_evolve_3
t$6 <- t$1
t$7 <- t$2
t$5 <- < t$6 t$7
t$16 <- not t$5
bt t$16 CellularAutomaton_evolve_4
bt t$5 CellularAutomaton_evolve_5
comment while-join
label CellularAutomaton_evolve_4
t$4 <- default Object
population_map <- t$3
t$15 <- population_map
t$0 <- self
return t$0
comment while-body
label CellularAutomaton_evolve_5
t$11 <- t$1
t$10 <- call cell_at_next_evolution t$11
t$12 <- t$3
t$3 <- call concat t$10
t$9 <- t$3
t$13 <- t$1
t$14 <- int 1
t$1 <- + t$13 t$14
t$8 <- t$1
jmp CellularAutomaton_evolve_3"
        "
comment start
label CellularAutomaton_evolve_0
t$1 <- default Int
t$2 <- call num_cells
t$3 <- default String
jmp CellularAutomaton_evolve_3
comment while-pred
label CellularAutomaton_evolve_3
t$6 <- t$1
t$7 <- t$2
t$5 <- < t$6 t$7
t$16 <- not t$5
bt t$16 CellularAutomaton_evolve_4
bt t$5 CellularAutomaton_evolve_5
comment while-join
label CellularAutomaton_evolve_4
population_map <- t$3
t$0 <- self
return t$0
comment while-body
label CellularAutomaton_evolve_5
t$11 <- t$1
t$10 <- call cell_at_next_evolution t$11
t$12 <- t$3
t$3 <- call concat t$10
t$13 <- t$1
t$14 <- int 1
t$1 <- + t$13 t$14
jmp CellularAutomaton_evolve_3"
    
//testcases()
let bigcase = "
comment start
label CellularAutomaton_evolve_0
t$1 <- default Int
t$2 <- call num_cells
t$3 <- default String
jmp CellularAutomaton_evolve_3
comment while-pred
label CellularAutomaton_evolve_3
t$6 <- t$1
t$7 <- t$2
t$5 <- < t$6 t$7
t$16 <- not t$5
bt t$16 CellularAutomaton_evolve_4
bt t$5 CellularAutomaton_evolve_5
comment while-join
label CellularAutomaton_evolve_4
t$4 <- default Object
population_map <- t$3
t$15 <- population_map
t$0 <- self
return t$0
comment while-body
label CellularAutomaton_evolve_5
t$11 <- t$1
t$10 <- call cell_at_next_evolution t$11
t$12 <- t$3
t$3 <- call concat t$10
t$9 <- t$3
t$13 <- t$1
t$14 <- int 1
t$1 <- + t$13 t$14
t$8 <- t$1
jmp CellularAutomaton_evolve_3"
