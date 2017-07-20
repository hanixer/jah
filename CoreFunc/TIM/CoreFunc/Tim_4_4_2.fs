module Tim_4_4_2

open Language
open Util

type FramePtr = 
    | FrameAddr of Addr
    | FrameInt of int
    | FrameNull

type ValueAMode =
    | FramePtr
    | IntVConst of int

type Op =
    | Add | Sub | Mul | Div | Neg
    | Eq | Ne | Lt | Le | Gt | Ge

type TimAMode =
    | Arg of int
    | Label of string
    | Code of Instruction list
    | IntConst of int
and Instruction =
    | Take of int * int
    | Move of int * TimAMode
    | Enter of TimAMode
    | Push of TimAMode
    | PushV of ValueAMode
    | Op of Op
    | Return
    | Cond of (Instruction list * Instruction list)

type Closure = (Instruction list * FramePtr)
type TimStack = Closure list

type TimValueStack = int list
type TimDump = DummyTimDump

type Frame = Closure list

type TimHeap = Heap<Frame>

type CodeStore = (Name * (Instruction list)) list

type TimStats =
    { Steps : int
      HeapAllocated : int }

type TimCompilerEnv = (Name * TimAMode) list

type TimState = 
    { Instrs : Instruction list
      Fptr : FramePtr
      Stack : TimStack
      VStack : TimValueStack
      Dump : TimDump
      Heap : TimHeap
      CStore : CodeStore
      Stats : TimStats
      Exception : System.Exception option }
      

let frameAlloc heap xs =
    let heap', addr = heapAlloc heap xs
    heap', (FrameAddr addr)

let frameGet heap fptr n : Closure =
    match fptr with
    | FrameAddr addr ->
        let f = heapLookup heap addr
        List.item (n - 1) f
    | _ ->
        failwith "FrameAddr is expected"

let frameUpdate heap fptr n closure =
    match fptr with
    | FrameAddr addr ->
        let frame = heapLookup heap addr
        [ List.take (n - 1) frame
          [closure]
          List.skip n frame ]
        |> List.concat
        |> heapUpdate heap addr        
    | _ ->
        failwith "FrameAddr is expected"

let frameList (f : Frame) : Closure list = f

let codeLookup (cstore : CodeStore) l =
    match listTryFindFirst l cstore with
    | Some (_, instrs) -> instrs
    | None ->
        failwithf "Attemp to jump to unknown label %s" l

let statInitial : TimStats =
    { Steps = 0; HeapAllocated = 0 }
let statIncSteps s = 
    { s with Steps = s.Steps + 1 }
let statGetSteps s = s.Steps
let statIncAllocations s n =
    { s with HeapAllocated = s.HeapAllocated + n }
let statGetAllocations s = s.HeapAllocated

let initialArgStack = [[], FrameNull]
let initialValueStack = []
let initialDump = DummyTimDump
let intCode = [PushV FramePtr; Return]

let emptyState = 
    { Instrs = [Enter (Label "main")]
      Fptr = FrameNull 
      Stack = initialArgStack
      VStack = initialValueStack
      Dump = initialDump
      Heap = heapEmpty
      CStore = []
      Exception = None
      Stats = statInitial }

let compiledPrimitives = 
    [  ]

let arithmeticOps = 
    [ "+", Add
      "-", Sub
      "*", Mul
      "/", Div
      "<", Lt ]

let isBCompilable = function
    | EAp (EAp (EVar op, _), _) -> List.map fst arithmeticOps  |> List.contains op
    | EAp (EAp (EAp (EVar "if", e1), e2), e3) -> true
    | ENum _ -> true
    | _ -> false

let opToOp op = 
    match Util.listTryFindFirst op arithmeticOps with
    | Some (_, x) ->  x
    | _ -> failwithf "Operation %s not found" op

let mkIndNode n = Code [Enter (Arg n)]

let rec compileA expr env d =
    match expr with
    | ENum n -> d, IntConst n
    | EVar v -> 
        match listTryFindFirst v env with
        | Some (_, x) -> d, x
        | None -> failwithf "Unknown variable '%s'" v
    | _ -> 
        let d1, instrs = compileR expr env d
        d1, (Code instrs)

and compileR expr env d =
    match expr with
    | _ when isBCompilable expr ->
        compileB expr env d [Return]
    | EAp (e1, e2) ->
        let d1, am = compileA e2 env d
        let d2, instrs = compileR e1 env d1
        d2, (Push am :: instrs)
    | EVar _ | ENum _ -> 
        let d1, am = compileA expr env d
        d1, [Enter am]
    | ELet (false, bindings, body) ->
        let n = List.length bindings
        let processBinding (d, ams) (v, e) =
            let d', am = compileA e env d
            d', (am :: ams)
        let dn, ams = List.fold processBinding (d + n, []) bindings
        let ams = List.rev ams
        let bindingsEnv = List.mapi (fun i (v, _) -> v, Arg (d + i + 1)) bindings
        let bodyEnv = List.append bindingsEnv env
        let d', bodyInstrs = compileR body bodyEnv dn
        let moves = List.mapi (fun i am -> Move (d + i + 1, am)) ams
        d', List.append moves bodyInstrs
    | ELet (true, bindings, body) ->
        let n = List.length bindings
        let bindingsEnv = List.mapi (fun i (v, _) -> v, mkIndNode (d + i + 1)) bindings
        let bodyEnv = List.append bindingsEnv env
        let processBinding (d, ams) (v, e) =
            let d', am = compileA e bodyEnv d
            d', (am :: ams)
        let dn, ams = List.fold processBinding (d + n, []) bindings
        let ams = List.rev ams
        let d', bodyInstrs = compileR body bodyEnv dn
        let moves = List.mapi (fun i am -> Move (d + i + 1, am)) ams
        d', List.append moves bodyInstrs
    | _ -> failwith "compileR cannot compile this yet"

and compileB expr env d cont =
    match expr with
    | EAp (EAp (EVar opVar, e1), e2) ->
        let cont1 = Op (opToOp opVar) :: cont
        let d1, cont2 = compileB e1 env d cont1
        compileB e2 env d1 cont2
    | EAp (EAp (EAp (EVar "if", e1), e2), e3) ->
        let d1, cont1 = compileB e2 env d cont
        let d2, cont2 = compileB e3 env d1 cont
        let cont3 = [ Cond (cont1, cont2) ]
        compileB e1 env d2 cont3
    | ENum n ->
        d, (PushV (IntVConst n) :: cont)
    | _ ->
        let d1, compiled = compileR expr env d
        d1, (Push (Code cont) :: compiled)

let extendEnvWithArgs env args : TimCompilerEnv =
    args 
    |> List.mapi (fun i arg -> arg, Arg (i + 1))
    |> List.append <| env

let compileSc env (name, args, body)  : Name * (Instruction list) =
    let n = List.length args
    let newEnv = extendEnvWithArgs env args
    let d, compiledBody = 
        compileR body newEnv n
    let instrs =
        if n = 0 then compiledBody
        else Take (d, n) :: compiledBody
    name, instrs

let compile program =
    let scDefs = List.append preludeDefs program
    let initialEnv : TimCompilerEnv = 
        [ for name, args, body in scDefs do yield name, Label name ]
        |> List.append <|
        [ for name, code in compiledPrimitives do yield name, Label name ]
    let compiledScDefs = List.map (compileSc initialEnv) scDefs
    let compiledCode = List.append compiledScDefs compiledPrimitives
    { Instrs = [Enter (Label "main")]
      Fptr = FrameNull 
      Stack = initialArgStack
      VStack = initialValueStack
      Dump = initialDump
      Heap = heapEmpty
      CStore = compiledCode
      Exception = None
      Stats = statInitial }

let timFinal state = state.Instrs.IsEmpty

let amToClosure am fptr heap cstore : Closure =
    match am with
    | Arg n -> frameGet heap fptr n
    | Code il -> il, fptr
    | Label l -> codeLookup cstore l, fptr
    | IntConst n -> intCode, FrameInt n

let take t n state =
    if List.length state.Stack < n then
        failwith "Too few args for Take instruction"
    let fromStack = List.take n state.Stack
    let empty = [], FrameNull
    let frame = Seq.replicate (t - n) empty |> List.ofSeq
    let heap', fptr' = 
        List.append fromStack frame
        |> frameAlloc state.Heap
    let stack = List.skip n state.Stack
    { state with
        Fptr = fptr'
        Stack = stack
        Heap = heap'
        Stats = statIncAllocations state.Stats n }

let move i am state =
    let closure = amToClosure am state.Fptr state.Heap state.CStore
    let heap = frameUpdate state.Heap state.Fptr i closure
    { state with Heap = heap }

let enter am state =
    if not state.Instrs.IsEmpty then 
        failwith "Enter must be last instruction"
    let code, fptr = amToClosure am state.Fptr state.Heap state.CStore
    { state with
        Instrs = code
        Fptr = fptr }

let push am state =
    { state with 
        Stack = amToClosure am state.Fptr state.Heap state.CStore :: state.Stack }

let pushv vm state =
    let fin n = { state with VStack = n :: state.VStack }
    match vm with
    | FramePtr ->
        match state.Fptr with
        | FrameInt n -> fin n
        | _ -> failwith "Integer is expected as frame pointer"
    | IntVConst n -> fin n

let oper op state =
    let f = 
        match op with
        | Add -> (+)
        | Sub -> (-)
        | Mul -> (*)
        | Div -> (/)
        | Lt -> fun x y -> if x < y then 0 else 1
        | _-> (+)
    match state.VStack with
    | n1 :: n2 :: vstack ->
        { state with VStack = (f n1 n2) :: vstack }
    | _ -> failwith "Expected two numbers on VStack"

let returnInstr state =
    if not state.Instrs.IsEmpty then 
        failwith "Return must be last instruction"

    match state.Stack with
    | (i, f) :: stack ->
        { state with
            Instrs = i
            Fptr = f
            Stack = stack }
    | _ -> failwith "Stack must be non-empty for return instruction"

let cond i1 i2 state =
    match state.VStack with
    | 0 :: vstack ->
        { state with Instrs = i1; VStack = vstack }
    | n :: vstack ->
        { state with Instrs = i2; VStack = vstack }
    | _ -> failwith "A number is expected for Cond instruction on VStack"

let dispatch = function
    | Take (t, n) -> take t n
    | Move (i, am) -> move i am
    | Enter am -> enter am
    | Push am -> push am
    | PushV vm -> pushv vm
    | Op op -> oper op
    | Return -> returnInstr
    | Cond (i1, i2) -> cond i1 i2

let step state =
    let tail = List.tail state.Instrs
    match state.Instrs with
    | instr :: instrs ->
        dispatch instr { state with Instrs = instrs }
    | _ -> failwith "Execution must be finished at this point"

let doAdmin s =
    { s with 
        Stats = statIncSteps s.Stats}

exception OverflowException

let rec eval state =
    printfn "eval now"        
    let rec go state states =
        if timFinal state then
            state :: states
        else
            try
                if statGetSteps state.Stats > 1000 then raise OverflowException
                let newState = state |> step |> doAdmin
                go newState (state :: states)
            with
            | e ->
                { state with Exception = Some e } :: states
    go state [] |> List.rev     

type HowMuchToPrint =
    | Full
    | Terse
    | None

let rec showArg d = function
    | Code il ->
        iStr "Code " |>iAppend<| showInstructions d il
    | x ->
        sprintf "%A" x |> iStr

and showInstruction d instr =
    match instr with
    | Take _ -> sprintf "%A" instr |> iStr
    | Enter am -> iStr "Enter " |>iAppend<| showArg d am
    | Push am -> iStr "Push " |>iAppend<| showArg d am
    | PushV vm -> sprintf "%A" instr |> iStr
    | Op _ -> sprintf "%A" instr |> iStr
    | Return -> iStr "Return"
    | Cond (il1, il2) ->
        [ iStr "Cond "; 
          [ showInstructions d il1;
            iNewline;
            showInstructions d il2 ]
          |> iConcat
          |> iIndent ]
        |> iConcat
    | Move (i, am) ->
        [ iStr "Move "; iNum i; iStr " " ]
        |> iConcat
        |>iAppend<| showArg d am
    | _ -> sprintf "%A" instr |> iStr

and showInstructions d il =
    let numTerse = 3
    match d with
    | None -> iStr "{..}"
    | Terse ->
        let instrs = List.map (showInstruction None) il
        let body =
            if List.length il <= numTerse then instrs
            else List.take numTerse instrs |>List.append<| [iStr ".."]
        iConcat [iStr "{"; iInterleave (iStr ", ") body |> iIndent; iStr "}"]
    | Full ->
        let sep = iStr "," |>iAppend<| iNewline
        let instrs = List.map (showInstruction Full) il
        iConcat [iStr "{"; iInterleave sep instrs |> iIndent; iStr "}"]

let showFramePtr fptr = sprintf "%A" fptr |> iStr

let showClosure (i, f) = 
    iConcat
        [ iStr "("; showInstructions Terse i; iStr ",";
          showFramePtr f; iStr ")" ]

let showDump dump = iNil

let showValueStack vstack =
    iConcat 
        [ iStr "VStack:["
          List.map iNum vstack |> iInterleave (iStr ", ")
          iStr "]" ]

let showStack stack =
    iConcat
        [ iStr "Arg stack: [";
          List.map showClosure stack |> iInterleave iNewline |> IIdent;
          iStr "]"; iNewline ]

let showFrame heap f =
    match f with
    | FrameNull -> iStr "Null frame ptr" |>iAppend<| iNewline
    | FrameAddr a ->
        iConcat
            [ iStr "Frame: <";
              heapLookup heap a
              |> frameList 
              |> List.map showClosure 
              |> iInterleave iNewline
              |> iIndent;
              iStr ">"; iNewline ]
    | FrameInt n ->
        iConcat [ iStr "Frame ptr (int): "; iNum n; iNewline ]

let showException = function
    | Some e ->
        [ iStr "Exception! "; e.ToString() |> iStr; iNewline; ]
        |> iConcat
    | _ -> iNil

let showState state =
    iConcat [
        iStr "Code:  "; showInstructions Terse state.Instrs; iNewline;
        showFrame state.Heap state.Fptr;
        showStack state.Stack;
        showValueStack state.VStack;
        showDump state.Dump;
        showException state.Exception;
        iNewline
    ]

let showSc (name, il) =
    iConcat
        [ iStr "Code for "; iStr name; iStr ":"; iNewline;
          iStr "   "; showInstructions Full il; iNewline; iNewline ]

let showScDefs state =
    List.map showSc state.CStore
    |> iInterleave iNewline

let showStats state =
    [ iStr "Steps taken = "; iNum (statGetSteps state.Stats); iNewline;
      iStr "Frames allocated =  "; iNum (heapSize state.Heap); iNewline;
      iStr "Total heap allocations = "; iNum (statGetAllocations state.Stats); iNewline ]
    |> iConcat 
        

let showFullResults states =
    let s  = List.head states
    iConcat
        [ iStr "Supercombinator definitions"; iNewline;
          showScDefs s; iNewline; iNewline;
          iStr "State transitions"; iNewline; iNewline;
          iLayn (List.map showState states); iNewline; iNewline;
          showStats (List.last states) ]
    |> iDisplay

let showResults states =
    [ showState (List.last states); iNewline; iNewline; 
      showStats (List.last states) ]
    |> iConcat
    |> iDisplay

let runProg<'p> = parse >> compile >> eval >> showResults

let fullRun<'p> = parse >> compile >> eval >> showFullResults