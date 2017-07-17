module Tim

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
    | Take of int
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

let compiledBinaryArithmetic name op = 
    let l2 = [ Op op; Return ]
    let l1 = [ Push (Code l2); Enter (Arg 1) ]
    let code = [ Take 2; Push (Code l1); Enter (Arg 2) ]
    name, code

let compiledPrimitives = 
    [ compiledBinaryArithmetic "+" Add
      compiledBinaryArithmetic "-" Sub
      compiledBinaryArithmetic "*" Mul
      compiledBinaryArithmetic "/" Div
      "if", [ Take 3; ] ] // Continue from here

let rec compileA expr env =
    let r () = compileR expr env |> Code
    match expr with
    | ENum n -> IntConst n
    | EVar v -> 
        match listTryFindFirst v env with
        | Some (_, x) -> x
        | None -> failwithf "Unknown variable '%s'" v
    | _ -> r ()

and compileR expr env =
    printfn "compileR %A" expr
    match expr with
    | EAp (e1, e2) ->
        Push (compileA e2 env) :: (compileR e1 env)
    | EVar _ | ENum _ -> [ compileA expr env |> Enter ]
    | _ -> failwith "compileR cannot compile this yet"

let extendEnvWithArgs env args : TimCompilerEnv =
    args 
    |> List.mapi (fun i arg -> arg, Arg (i + 1))
    |> List.append <| env

let compileSc env (name, args, body)  : Name * (Instruction list) =
    let n = List.length args
    let newEnv = extendEnvWithArgs env args
    let compiledBody = 
        compileR body newEnv
    let instrs =
        if n = 0 then compiledBody
        else Take n :: compiledBody
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

let take n state =
    if List.length state.Stack >= n then
        let heap', fptr' = frameAlloc state.Heap (List.take n state.Stack)
        let stack = List.skip n state.Stack
        { state with
            Fptr = fptr'
            Stack = stack
            Heap = heap'
            Stats = statIncAllocations state.Stats n }
    else
        failwith "Too few args for Take instruction"

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
    | Take n -> take n
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
                if statGetSteps state.Stats > 100 then raise OverflowException
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
    | Take n -> sprintf "%A" instr |> iStr
    | Enter am -> iStr "Enter " |>iAppend<| showArg d am
    | Push am -> iStr "Push " |>iAppend<| showArg d am
    | PushV vm -> sprintf "%A" instr |> iStr
    | Op _ -> sprintf "%A" instr |> iStr
    | Return -> iStr "Return"
    | Cond _ -> iStr "Cond"

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

let showState state =
    iConcat [
        iStr "Code:  "; showInstructions Terse state.Instrs; iNewline;
        showFrame state.Heap state.Fptr;
        showStack state.Stack;
        showValueStack state.VStack;
        showDump state.Dump;
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
    [ showState (List.last states); iNewline; iNewline; showStats (List.last states) ]
    |> iConcat
    |> iDisplay

let runProg<'p> = parse >> compile >> eval >> showResults

let fullRun<'p> = parse >> compile >> eval >> showFullResults