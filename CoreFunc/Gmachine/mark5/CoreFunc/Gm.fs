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
    | Cond of GmCode * GmCode

and GmCode = Instruction list

type GmStack = Addr list

type Node =
    | NAp of Addr * Addr
    | NNum of int
    | NGlobal of int * GmCode
    | NInd of Addr

type GmHeap = Heap<Node>

type GmStats = int

type GmEnvironment = Name * int list

type GmDump = (GmCode * GmStack) list

type GmState =
    { Code : GmCode
      Heap : GmHeap
      Globals : GmGlobals
      Stack : GmStack
      Dump : GmDump
      Stats : GmStats }

type CompiledSC = Name * int * GmCode
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

let emptyState = 
    { Code = []; Heap = heapEmpty; Globals = []; Stack = []; Stats = 0; Dump = [] }

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
        [ iStr "["; 
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

let showDumpItem (code, stack) =
    iConcat
        [ iStr "<"
          showShortInstruction 3 code
          iStr ","
          showShortStack stack
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

let showState s =
    iConcat
        [ showStack s; iNewline; 
          showDump s; iNewline;
          showInstructions s.Code; iNewline ]

let showStats s =
    iConcat 
        [ iStr "Steps taken:"; s.Stats |> statGetSteps |> iNum ]

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

let compiledArithmetic str op =
    str, 2, [Push 1; Eval; Push 1; Eval; op; Update 2; Pop 2; Unwind]

let compiledCompirison str op =
    str, 2, [Push 1; Eval; Push 1; Eval; op; Update 2; Pop 2; Unwind]

let buildInDyadic =
    [   ("+", Add)
        ("-", Sub)
        ("*", Mul)
        ("/", Div)
        ("==", Eq)
        ("˜=", Ne)
        (">=", Ge)
        (">", Gt)
        ("<=", Le)
        ("<", Lt) ]

let compiledPrimitives = 
    [ compiledArithmetic "+" Add
      compiledArithmetic "-" Sub
      compiledArithmetic "*" Mul
      compiledArithmetic "/" Div
      "negate", 1, [Push 0; Eval; Neg; Update 1; Pop 1; Unwind]
      compiledArithmetic "==" Eq
      compiledArithmetic "~=" Ne
      compiledArithmetic "<" Lt
      compiledArithmetic "<=" Le
      compiledArithmetic ">" Gt
      compiledArithmetic ">=" Ge
      "if", 3, [Push 0; Eval; Cond ([Push 1], [Push 2]); Update 3; Pop 3; Unwind] ]

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
    let heap', a = heapAlloc s.Heap (NNum (if b then 1 else 0))
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

let arithmetic2 = primitive2 boxInteger unboxInteger

let comparison = primitive2 boxBoolean unboxInteger

let allocateSc heap (name, nargs, is) =
    let heap', addr = heapAlloc heap (NGlobal (nargs, is))
    heap', (name, addr)

let initialCode = [ Pushglobal "main"; Eval ]
// let initialCode = [ Pushglobal "main"; Unwind ]

let argOffset n env =
    List.map (fun (name, m) -> (name, m + n)) env

let getArg = function
    | NAp (_, a) -> a
    | _ -> failwith "application node is expected"

let compileLetDefs comp defs env =
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
    

let compileLet compArg compBody defs body env =
    let defsIs = compileLetDefs compArg defs env
    let n = (List.length defs)
    let newEnv = buildLetEnv env defs
    List.concat
        [ defsIs
          compBody body newEnv
          [Slide n] ]

let compileLetRecDefs comp defs env =
    let compileDef (is, n) (v, e) =
        let is' = List.concat [ is; comp e env; [Update (n - 1)] ]
        is' , (n - 1)
    List.fold compileDef ([], List.length defs) defs 
    |> fst

let compileLetRec compArg compBody defs body env =
    let n = List.length defs
    let newEnv = buildLetEnv env defs
    let defsIs = compileLetRecDefs compArg defs newEnv
    List.concat
        [ [Alloc n]
          defsIs
          compBody body newEnv
          [Slide n] ]

let rec compileC expr env =
    match expr with
    | EVar v ->
        match List.tryFind (fst >> ((=) v)) env with
        | Some (_, n) -> [Push n]
        | None -> [Pushglobal v]
    | ENum n -> [Pushint n]
    | EAp (e1, e2) ->
        List.concat
            [ compileC e2 env
              compileC e1 (argOffset 1 env)
              [Mkap] ]
    | ELet (false, defs, body) ->
        compileLet compileC compileC defs body env
    | ELet (true, defs, body) ->
        compileLetRec compileC compileC defs body env
    | _ ->
        failwithf "cannot compile %A" expr

let compileAp compPrimitive compOther e1 e2 env  =
    match e1 with
    | EVar "negate" ->
        List.append (compPrimitive e2 env) [Neg]
        |> Some
    | EAp(EAp(EVar "if", cond), ethen) ->
        List.concat
            [ compPrimitive cond env
              [Cond (compPrimitive ethen env, compPrimitive e2 env)] ]
        |> Some
    | EAp (EVar op, lhs) -> 
        match List.tryFind (fst >> ((=) op)) buildInDyadic with
        | Some (_, opInstr) ->
            List.concat
                [ compPrimitive e2 env
                  compPrimitive lhs (argOffset 1 env)
                  [opInstr] ]
            |> Some
        | _ -> None
    | _ -> None

let rec compileE expr env =
    match expr with
    | ENum n -> [Pushint n]
    | EAp (e1, e2) ->
        match compileAp compileE compileC e1 e2 env with
        | Some res -> res
        | None ->
            List.append (compileC expr env) [Eval]
    | ELet (false, defs, body) ->
        compileLet compileC compileE defs body env
    | ELet (true, defs, body) ->
        compileLetRec compileC compileE defs body env
    | _ ->
        compileC expr env

let compileR expr env = 
    let d = List.length env
    compileE expr env 
    |> List.append
    <| [ Update d; Pop d; Unwind ]

let compileSc (name, env, body) =
    let newEnv = List.mapi (fun i x -> x, i) env
    let compiled = compileR body newEnv 
    name, env.Length, compiled

let buildInitialHeap program = 
    let compiled = 
        List.map compileSc (List.append preludeDefs program)
        |> List.append compiledPrimitives
    mapAccumul allocateSc heapEmpty compiled

let compile program =
    let heap, globals = buildInitialHeap program
    { Code = initialCode
      Heap = heap
      Globals = globals
      Stats = statInitial
      Dump = []
      Stack = [] }


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

let unwind s =
    let a = (List.head s.Stack)
    match heapLookup s.Heap a with
    | NNum n ->
        match s.Dump with
        | (i', stack') :: dump ->
            { s with 
                Code = i'
                Stack = a :: stack'
                Dump = dump }
        | _ ->
            failwith "evaluation has to be terminated at this point"
    | NAp (a1, a2) -> 
        { s with
            Stack = a1 :: s.Stack 
            Code = [Unwind] }
    | NGlobal (n, c) ->
        if s.Stack.Length < n then
            let code, stack = List.head s.Dump
            { s with
                Code = code
                Stack = List.last s.Stack :: stack
                Dump = List.tail s.Dump }
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
        Dump = (s.Code, List.tail s.Stack) :: s.Dump }

let neg = arithmetic1 (~-)

let cond i1 i2 s =
    match s.Stack with
    | a :: stack ->
        match heapLookup s.Heap a with
        | NNum 1 -> 
            { s with
                Stack = stack
                Code = List.append i1 s.Code }
        | NNum 0 -> 
            { s with
                Stack = stack
                Code = List.append i2 s.Code }
        | _ -> 
            failwith "Expected 1 or 0 on the top of the stack"
    | _ ->
        failwith "expected nonempty list for conditional"

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
    | Cond (i1, i2) -> cond i1 i2

let step (s : GmState) = 
    match s.Code with
    | i :: is -> dispatch i { s with Code = is }
    | _ -> failwith "expected nonempty code in step"

let doAdmin (s : GmState) =
    { s with 
        Stats = statIncSteps s.Stats}

let rec eval state =        
    let rec go state states =
        if gmFinal state then
            states
        else
            let newState = state |> step |> doAdmin
            go newState (state :: states)
    go state [] |> List.rev

let runProg<'p> = parse >> compile >> eval >> showResults