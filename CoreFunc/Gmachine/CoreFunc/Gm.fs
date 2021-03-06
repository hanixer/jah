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
    | Update of int
    | Pop of int
    | Slide of int
    | Alloc of int
    | Eval
    | Add | Sub | Mul | Div | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    | And | Or | Not
    | Cond of GmCode * GmCode
    | Pack of int * int
    | Casejump of (int * GmCode) list
    | Split of int
    | Print
    | Pushbasic of int
    | Mkbool
    | Mkint
    | Get
    | Return
    | UpdateInt of int


and GmCode = Instruction list

type GmStack = Addr list

type Node =
    | NAp of Addr * Addr
    | NNum of int
    | NGlobal of int * GmCode
    | NInd of Addr
    | NConstr of int * (Addr list)

type GmHeap = Heap<Node>

type GmStats = int

type GmEnvironment = (Name * int) list

type GmVStack = int list

type GmDump = (GmCode * GmStack * GmVStack) list

type GmOutput = char list

type GmState =
    { Code : GmCode
      Heap : GmHeap
      Globals : GmGlobals
      Stack : GmStack
      Dump : GmDump
      Stats : GmStats
      Output : GmOutput
      Exception : System.Exception option
      VStack : GmVStack }

type CompiledSC = Name * int * GmCode
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

let emptyState = 
    { Code = []; Heap = heapEmpty; Globals = []; Stack = []; Stats = 0; Dump = []; Output = []; Exception = None; VStack = [] }

let statInitial = 0
let statIncSteps s = s + 1
let statGetSteps s = s

let rec getLeftmost s a =
    match heapLookup s.Heap a with
    | NAp (a1, a2) ->
        getLeftmost s a1
    | n -> n, a

let rec showNode s a = function
    | NNum n -> iNum n
    | NGlobal (n, g) ->
        let v, _ = List.find (snd >> ((=) a)) s.Globals
        iConcat
            [ iStr "Global "; iStr v ]
    | NAp (a1, a2) ->
        let leftmost, addrLeft = getLeftmost s a1
        iConcat
            [ iStr "Ap "; showAddr a1;
              iStr " "; showAddr a2;
              iStr " /"; showNode s addrLeft leftmost; iStr "/" ]
    | NInd a ->
        iConcat
            [ iStr "Ind "; showAddr a
              iStr " ("; heapLookup s.Heap a |> showNode s a; iStr ")" ]
    | NConstr (t, addrs) ->
        iConcat
            [ iStr "Constr "
              iNum t
              iStr " ["
              List.map showAddr addrs |> iInterleave (iStr ", ")
              iStr "]"]

let showStackItem s a =
    iConcat
        [ showAddr a; iStr ": ";
          heapLookup s.Heap a |> showNode s a ]

let showStack s =
    iConcat
        [ iStr "Stack:[";
          s.Stack 
          |> List.rev 
          |> List.map (showStackItem s) 
          |> iInterleave iNewline 
          |> iIndent;
          iStr "]"; ]

let showShortStack stack =
    iConcat
        [ iStr "S:["; 
          List.map (string >> iStr) stack |> iInterleave (iStr ","); 
          iStr "]" ]

let showInstruction i = sprintf "%A" i |> iStr

let showInstructions code =
    iConcat 
        [ iStr "Code:{";
          code |> List.map showInstruction |> iInterleave iNewline |> iIndent;
          iStr "}"; iNewline ]

let showShortInstruction n code =
    let minNumber = min n (List.length code)
    let codes = List.take minNumber code |> List.map showInstruction
    let dotcodes = 
        if List.length code > n then
            List.append codes [iStr "..."]
        else codes
    iConcat
        [ iStr "{"
          iInterleave (iStr "; ") dotcodes
          iStr "}" ]

let showVStack s =
    iConcat 
        [ iStr "VStack:["
          List.map iNum s.VStack |> iInterleave (iStr ", ")
          iStr "]" ]

let showShortVStack vstack =
    iConcat 
        [ iStr "V:["
          List.map iNum vstack |> iInterleave (iStr ", ")
          iStr "]" ]

let showDumpItem (code, stack, vstack) =
    iConcat
        [ iStr "<"
          showShortInstruction 3 code
          iStr ", "
          showShortStack stack
          iStr ", "
          showShortVStack vstack
          iStr ">"]

let showDump s =
    iConcat 
        [ iStr "Dump:[";
          s.Dump
          |> List.rev
          |> List.map showDumpItem
          |> iInterleave iNewline
          |> IIdent;
          iStr "]" ]

let showExcept s =
    match s.Exception with
    | Some e ->
        iConcat [ iStr "Exception! "; e.ToString() |> iStr; iNewline ]
    | None -> iNil

let showState s =
    iConcat
        [ showVStack s; iNewline;
          showStack s; iNewline; iNewline;
          showDump s; iNewline; iNewline;
          showInstructions s.Code;
          showExcept s; ]

let showStats s =
    iConcat 
        [ iStr "Steps taken:"; s.Stats |> statGetSteps |> iNum; iNewline;
          iStr "Heap size:"; heapSize s.Heap |> iNum ]

let showSc s (name, addr) =
    let arity, code = 
        match heapLookup s.Heap addr with 
        | NGlobal (x,y) -> x,y
        | _ -> failwith "wrong node, expected global"
    iConcat 
        [ iStr "Code for "; iStr name; iNewline;
          showInstructions code; iNewline; iNewline ]

let showResults states =
    let s  = List.head states
    iConcat   
        [ iStr "Supercombinator definitions"; iNewline;
          iInterleave iNewline (List.map (showSc s) s.Globals); iNewline; iNewline; 
          iStr "State transitions"; iNewline; iNewline;
          iLayn (List.map showState states); iNewline; iNewline;
          showStats (List.last states) ]
    |> iDisplay

let lookupStackTop s =
    List.head s.Stack |> heapLookup s.Heap

let mkBinPrimitive op =
    op, ["x"; "y"], (EAp (EAp (EVar op, EVar "x"), EVar "y"))

let primitives =
    [ mkBinPrimitive "+"
      mkBinPrimitive "-"
      mkBinPrimitive "*"
      mkBinPrimitive "/"
      mkBinPrimitive "=="
      mkBinPrimitive "~="
      mkBinPrimitive "<"
      mkBinPrimitive "<="
      mkBinPrimitive ">"
      mkBinPrimitive ">="
      mkBinPrimitive "&"
      mkBinPrimitive "|"
      "negate", ["x"], EAp (EVar "negate", EVar "x")
      "if", ["c"; "t"; "f"], (EAp (EAp (EAp (EVar "if", EVar "c"), EVar "t"), EVar "f"))
      "True", [], EConstr (2, 0)
      "False", [], EConstr (1, 0) ]


let binaryOpsToInstr = 
    [ "+", Add
      "-", Sub
      "*", Mul
      "/", Div
      "==", Eq
      "~=", Ne
      "<", Lt
      "<=", Le
      ">", Gt
      ">=", Ge
      "&", And
      "|", Or ]
      |> Map.ofList

let isPrimitiveBinaryOp op =
    Map.containsKey op binaryOpsToInstr

let primitiveBinaryToInstr op =
    Map.find op binaryOpsToInstr

let getMkInstr op =
    if ["+"; "-"; "*"; "/"] |> List.contains op then
        Mkint
    else Mkbool

let boxInteger n s =
    let heap', a = heapAlloc s.Heap (NNum n)
    { s with
        Heap = heap'
        Stack = a :: s.Stack }

let unboxInteger a s =
    match heapLookup s.Heap a with
    | NNum n -> n
    | _ -> failwith "Number node is expected"

let boxBoolean b s =
    let t = if b then 2 else 1
    let heap', a = heapAlloc s.Heap (NConstr (t, []))
    { s with
        Heap = heap'
        Stack = a :: s.Stack }

let primitive1 box unbox op state =
    match state.Stack with
    | a :: stack ->
        unbox a state |> op |> box <| { state with Stack = stack }
    | _ ->
        failwith "Stack is expected to be nonempty"

let primitive2 box unbox op state =
    match state.Stack with
    | a1 :: a2 :: stack ->
        let p1 = unbox a1 state
        let p2 = unbox a2 state
        op p1 p2 |> box <| { state with Stack = stack }
    | _ -> failwith "stack expected to have 2 arguments"

let arithmetic1 = primitive1 boxInteger unboxInteger

let binary f s =
    match s.VStack with
    | n0 :: n1 :: v ->
        { s with VStack = f n0 n1 :: v }
    | _ ->
        failwith "two numbers expected on VStack"

let arithmetic2 = binary

let boolToInt x = if  x then 2 else 1

let intToBool = function
    | 2 -> true
    | 1 -> false
    | _ -> failwith "2 or 1 expected"

let comparison f = 
    let func x y = boolToInt (f x y)
    binary func

let boolean f =
    binary (fun x y -> f (intToBool x) (intToBool y) |> boolToInt)     

let notInstr s = 
    match s.VStack with
    | n :: ns ->
        let newValue = n |> intToBool |> not |> boolToInt
        { s with VStack = newValue :: ns }
    | _ ->
        failwith "Empty VStack"   

let allocateSc heap (name, nargs, is) =
    let heap', addr = heapAlloc heap (NGlobal (nargs, is))
    heap', (name, addr)

let initialCode = [ Pushglobal "main"; Eval ]

let argOffset n (env : GmEnvironment) =
    List.map (fun (name, m) -> (name, m + n)) env

let getArg = function
    | NAp (_, a) -> a
    | _ -> failwith "application node is expected"

let compileLetDefs comp defs (env : GmEnvironment) =
    let compileDef (is, env) (v, e) =
        comp e env |> List.append is, argOffset 1 env
    List.fold compileDef ([], env) defs 
    |> fst

let buildLetEnv env defs =
    let defVars =
        List.map fst defs 
        |> List.rev 
        |> List.mapi (fun i x -> x, i)
        |> List.rev
    let envOffset = argOffset (List.length defs) env
    List.append envOffset defVars

let compileAlt comp env (t, vars, body)  =
    let n = List.length vars
    let varsEnv = List.mapi (fun i x -> x, i) vars
    let newEnv = List.append varsEnv (argOffset n env)
    let compiled = 
        List.concat
            [ [Split n]
              comp body newEnv
              [Slide n] ]
    t, compiled

let compileD comp alts env =
    List.map (compileAlt comp env) alts

let compileLet (compArg : GmCompiler) (compBody : GmCompiler) lastInstr defs body  env =
    let defsIs = compileLetDefs compArg defs env
    let n = (List.length defs)
    let newEnv = buildLetEnv env defs
    List.concat
        [ defsIs
          compBody body newEnv
          lastInstr n ]

let compileLetRecDefs comp defs env =
    let compileDef (is, n) (v, e) =
        let is' = List.concat [ is; comp e env; [Update (n - 1)] ]
        is' , (n - 1)
    List.fold compileDef ([], List.length defs) defs 
    |> fst

let compileLetRec (compArg : GmCompiler) (compBody : GmCompiler) lastInstr defs body env =
    let n = List.length defs
    let newEnv = buildLetEnv env defs
    let defsIs = compileLetRecDefs compArg defs newEnv
    List.concat
        [ [Alloc n]
          defsIs
          compBody body newEnv
          lastInstr n ]

let rec extractPackExpr = function
    | EConstr (t, a) ->
        t, a, []
    | EAp (e1, e2) ->
        let t, a, exprs = extractPackExpr e1
        t, a, (e2 :: exprs)
    | _ ->
        failwith "Only EConstr or EAp is expected"

let rec isPackExpr expr =
    let rec go n = function
        | EConstr (_, a) when a = n -> true
        | EAp (e1, _) -> go (n + 1) e1
        | _ -> false
    go 0 expr

let compileConstr (comp : GmCompiler) expr (env : GmEnvironment)  =
    let t, a, exprs = extractPackExpr expr
    let compileSub (instrs, en : GmEnvironment) expr = 
        let compiled = comp expr en
        (List.append instrs compiled), (argOffset 1 en)
    let compiled = List.fold compileSub ([], env) exprs |> fst
    List.concat
        [ compiled
          [Pack (t, a)] ]

let compileIf (compCond : GmCompiler) (compBody : GmCompiler) c t e env =
    List.concat
        [ compCond c env
          [ Cond (compBody t env, compBody e env) ] ]

let compileCase (comp : GmCompiler) condExpr alts env =
    List.concat
        [ comp condExpr env
          [compileD comp alts env |> Casejump] ]


let rec compileC expr env =
    match expr with
    | EVar v ->
        match List.tryFind (fst >> ((=) v)) env with
        | Some (_, n) -> [Push n]
        | None -> [Pushglobal v]
    | ENum n -> [Pushint n]
    | _ when isPackExpr expr ->
        compileConstr compileC expr env
    | EAp (e1, e2) ->
        List.concat
            [ compileC e2 env
              compileC e1 (argOffset 1 env)
              [Mkap] ]
    | ELet (false, defs, body) ->
        let last n = [Slide n]
        compileLet compileC compileC last defs body  env
    | ELet (true, defs, body) ->
        let last n = [Slide n]
        compileLetRec compileC compileC last defs body env
    | _ ->
        failwithf "cannot compile %A" expr

let rec compileE expr env =
    let defaultComp () =
         List.append (compileC expr env) [Eval]
    match expr with
    | ENum n -> [Pushint n]
    | _ when isPackExpr expr ->
        compileConstr compileC expr env
    | EAp (EAp (EVar op, e1), e2) when isPrimitiveBinaryOp op ->
        let mkInstr = getMkInstr op
        List.concat
            [ compileB expr env
              [mkInstr] ]
    | EAp (EAp (EAp (EVar "if", c), t), e) ->
        compileIf compileB compileE c t e env
    | EAp (EVar "negate", e) ->
        List.concat
            [ compileB expr env
              [Mkint] ]
    | ELet (false, defs, body) ->
        let last n = [Slide n]
        compileLet compileC compileE last defs body env
    | ELet (true, defs, body) ->
        let last n = [Slide n]
        compileLetRec compileC compileE last defs body env
    | ECase (condExpr, alts) ->
        compileCase compileE condExpr alts env
    | _ ->
        defaultComp ()

and compileB expr env =
    match expr with
    | ENum n -> [Pushbasic n]
    | ELet (false, defs, body) ->
        let last n = [Pop n]
        compileLet compileC compileB last defs body env
    | ELet (true, defs, body) ->
        let last n = [Pop n]
        compileLetRec compileC compileB last defs body env
    | EAp (EAp (EVar op, e1), e2) when isPrimitiveBinaryOp op ->
        let instr = primitiveBinaryToInstr op
        List.concat
            [ compileB e2 env
              compileB e1 env
              [instr] ]
    | EAp (EAp (EAp (EVar "if", c), t), e) ->
        compileIf compileB compileB c t e env
    | EAp (EVar "negate", e) ->
        List.concat
            [ compileB e env
              [Neg] ]
    | EAp (EVar "not", e) ->
        List.concat
            [ compileB e env
              [Not] ]        
    | _ ->
        List.concat
            [ compileE expr env
              [Get] ]

let rec compileR d expr env = 
    match expr with
    | ELet (isRec, defs, body) ->
        let last _ = []
        let d' = d + List.length defs
        let compLet = if isRec then compileLetRec else compileLet
        compLet compileC (compileR d') last defs body env
    | EAp (EAp (EAp (EVar "if", c), t), e) ->
        compileIf compileB (compileR d) c t e env
    | ECase (condExpr, alts) ->
        let d' = d + List.length alts
        compileCase (compileR d') condExpr alts env
    | _ ->
        let compiled = compileE expr env
        match List.tryLast compiled with
        | Some Mkint ->
            let compiled = List.take (List.length compiled - 1) compiled
            List.append compiled [UpdateInt d; Pop d; Unwind]
        | _ ->
            List.append compiled [Update d; Pop d; Unwind]

let compileSc (name, env, body) =
    let newEnv = List.mapi (fun i x -> x, i) env
    let d = List.length newEnv
    let compiled = compileR d body newEnv 
    name, env.Length, compiled

let buildInitialHeap program = 
    let compiled = 
        List.concat [preludeDefs; program; primitives]
        |> List.map compileSc
    mapAccumul allocateSc heapEmpty compiled

let compile program =
    let heap, globals = buildInitialHeap program
    { emptyState with
        Code = initialCode
        Heap = heap
        Globals = globals
        Stats = statInitial }


let gmFinal (s : GmState) =
    s.Code.IsEmpty && s.Dump.IsEmpty

let tryFindGlobal s name =
    List.tryFind (fst >> ((=) name)) s.Globals

let pushglobal f (s : GmState) =
    match tryFindGlobal s f with
    | Some (_, x) ->
        { s with Stack = x :: s.Stack }
    | _ ->
        failwithf "cannot find global %s" f

let pushint (n : int) (s : GmState) =
    let name = (string n)
    match tryFindGlobal s name with
    | Some (_, x) -> 
        { s with Stack = x :: s.Stack }
    | None ->
        let newHeap, a = heapAlloc s.Heap (NNum n)
        let newGlobals = (name, a) :: s.Globals
        let newStack = a :: s.Stack
        { s with
            Heap = newHeap
            Globals = newGlobals
            Stack = newStack }

let mkap (s : GmState) =
    match s.Stack with
    | a1 :: a2 :: stackTail ->
        let newHeap, a = heapAlloc s.Heap (NAp (a1, a2))
        let newStack =  a :: stackTail
        { s with 
            Stack = newStack
            Heap = newHeap }
    | _ -> failwith "Two arguments expected in stack!"

let push n s =
    let argAddr = s.Stack |> List.item n
    { s with Stack = argAddr :: s.Stack }

let slide n s =
    let a = List.head s.Stack
    let rest = List.skip (n + 1) s.Stack
    { s with Stack = a :: rest }

let rearrange n heap stack =
    let tail = stack |> List.tail
    let args = tail |> List.take n |> List.map (heapLookup heap >> getArg)
    List.skip n stack |> List.append args 

let restoreDump state i s v d a =
    { state with 
        Code = i
        Stack = a :: s
        VStack = v
        Dump = d }

let unwind s =
    let a = (List.head s.Stack)
    match heapLookup s.Heap a with
    | NNum _ | NConstr _ ->
        match s.Dump with
        | (i, stack, vstack) :: dump ->
            restoreDump s i stack vstack dump a
        | _ ->
            failwith "evaluation has to be terminated at this point"
    | NAp (a1, a2) -> 
        { s with
            Stack = a1 :: s.Stack 
            Code = [Unwind] }
    | NGlobal (n, c) ->
        printfn "global: n = %d, stack length = %d" n s.Stack.Length
        if s.Stack.Length - 1 < n then
            let i, stack, vstack = List.head s.Dump
            restoreDump s i stack vstack (List.tail s.Dump) (List.last s.Stack)
        else
            { s with 
                Code = c
                Stack = rearrange n s.Heap s.Stack }
    | NInd a ->
        { s with 
            Stack = a :: List.tail s.Stack
            Code = [Unwind] }

let updateInstr n s =
    let a = List.head s.Stack
    let tail = List.tail s.Stack
    let an = List.item n tail
    let newHeap = heapUpdate s.Heap an (NInd a)
    { s with
        Stack = tail
        Heap = newHeap }

let pop n s =
    { s with
        Stack = List.skip n s.Stack }

let rec alloc n s =
    if n = 0 then s
    else
        let heap', a = heapAlloc s.Heap (NInd heapNull)
        alloc (n - 1) { s with Heap = heap'; Stack = a :: s.Stack }

let evalInstruction s = 
    { s with
        Code = [Unwind]
        Stack = [List.head s.Stack]
        Dump = (s.Code, List.tail s.Stack, s.VStack) :: s.Dump
        VStack = [] }

let neg s = 
    match s.VStack with
    | n :: ns ->
        { s with VStack = (-n) :: ns }
    | _ ->
        failwith "Empty VStack"

let cond i1 i2 s =
    let cont i v =
        { s with 
            Code = List.append i s.Code
            VStack = v }

    match s.VStack with
    | 2 :: v -> cont i1 v
    | 1 :: v -> cont i2 v
    | _ -> 
        failwith "2 or 1 is expected on the stop of the VStack"

let pack t n s =
    let v = NConstr (t, List.take n s.Stack)
    let heap', a = heapAlloc s.Heap v
    { s with
        Stack = a :: (List.skip n s.Stack)
        Heap = heap' }

let casejump alts s =
    let pred t' = fst >> ((=) t')
    match lookupStackTop s with
    | NConstr (t', addrs) ->
        match List.tryFind (pred t') alts with
        | Some (_, instrs) ->
            { s with Code = List.append instrs s.Code }
        | _ ->
            failwithf "Alternitive not found for tag %d" t'
    | _ -> 
        failwith "NConstr node is expected on the top of the stack"

let split n s =
    match lookupStackTop s with
    | NConstr (t, addrs) ->
        { s with Stack = List.append addrs (List.tail s.Stack) }
    | _ ->
        failwith "NConstr is expected on the top of the stack"

let printInstr s =
    match lookupStackTop s with
    | NNum n ->
        { s with
            Output = n |> string |> Seq.toList |> List.append s.Output }
    | NConstr (t, addrs) ->
        let n = List.length addrs 
        let i' = List.replicate n [Eval; Print] |> List.concat
        { s with
            Code = List.append i' s.Code 
            Stack = List.append addrs (List.tail s.Stack) }
    | _ ->
        failwith "Num or Contr is expected for print"

let pushbasic n s =
    { s with VStack = n :: s.VStack }

let mkbool s =
    match s.VStack with
    | t :: v ->
        let heap', a = heapAlloc s.Heap (NConstr (t, []))
        {s with
            Heap = heap'
            Stack = a :: s.Stack
            VStack = v }
    | _ ->
        failwith "A number is expected on the VStack"

let mkint s =
    match s.VStack with
    | t :: v ->
        let heap', a = heapAlloc s.Heap (NNum t)
        {s with
            Heap = heap'
            Stack = a :: s.Stack
            VStack = v }
    | _ ->
        failwith "A number is expected on the VStack"

let updateInt n state =
    match state.VStack, state.Stack with
    | (num :: v), s ->
        let heap1, b = heapAlloc state.Heap (NNum num)
        let heap2 = heapUpdate heap1 (List.item n s) (NInd b)
        { state with
            Heap = heap2
            VStack = v }
    | _, _ ->
        failwith "Expected an item on vstack"

let get s =
    match lookupStackTop s with
    | NConstr (n, []) | NNum n ->
        { s with
            Stack = List.tail s.Stack
            VStack = n :: s.VStack }
    | _ -> failwith "Get expects NConstr or NNum" 

let returnInstr state =
    match state.Dump with
    | (i, s, v) :: d ->
        { state with
            Code = i
            Stack = (List.last state.Stack) :: s
            Dump = d }
    | _ ->
        failwith "Expected nonempty dump"

let dispatch (i : Instruction) =
    match i with
    | Pushglobal f -> pushglobal f
    | Pushint n -> pushint n
    | Mkap -> mkap
    | Push n -> push n
    | Update n -> updateInstr n
    | Pop n -> pop n
    | Unwind -> unwind
    | Slide n -> slide n
    | Alloc n -> alloc n
    | Eval -> evalInstruction
    | Neg -> neg
    | Add -> arithmetic2 (+)
    | Sub -> arithmetic2 (-)
    | Mul -> arithmetic2 (*)
    | Div -> arithmetic2 (/)
    | Eq  -> comparison (=)
    | Ne  -> comparison (<>)
    | Lt  -> comparison (<)
    | Le  -> comparison (<=)
    | Gt  -> comparison (>)
    | Ge  -> comparison (>=)
    | And -> boolean (&&)
    | Or -> boolean (||)
    | Not -> notInstr
    | Cond (i1, i2) -> cond i1 i2
    | Pack (t, n) -> pack t n
    | Casejump alts -> casejump alts
    | Split n -> split n
    | Print -> printInstr
    | Pushbasic n -> pushbasic n
    | Mkbool -> mkbool
    | Mkint -> mkint
    | Get -> get
    | Return -> returnInstr
    | UpdateInt n -> updateInt n

let step (s : GmState) = 
    match s.Code with
    | i :: is -> dispatch i { s with Code = is }
    | _ -> failwith "expected nonempty code in step"

let doAdmin (s : GmState) =
    { s with 
        Stats = statIncSteps s.Stats}

exception OverflowException

let rec eval state =        
    let rec go state states =
        if gmFinal state then
            states
        else
            try
                if state.Stats > 1000 then raise OverflowException
                printfn "Step #%d" state.Stats                
                let newState = state |> step |> doAdmin
                go newState (state :: states)
            with
            | e ->
                { state with Exception = Some e } :: states
    go state [] |> List.rev        

let runProg<'p> = parse >> compile >> eval >> showResults