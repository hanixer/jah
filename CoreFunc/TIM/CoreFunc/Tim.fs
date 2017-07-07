module Tim

open Language
open Util

type TimAMode =
    | Arg of int
    | Label of string
    | Code of Instruction list
    | IntConst of int
and Instruction =
    | Take of int
    | Enter of TimAMode
    | Push of TimAMode

type FramePtr = 
    | FrameAddr of Addr
    | FrameInt of int
    | FrameNull

type Closure = (Instruction list * FramePtr)
type TimStack = Closure list

type TimValueStack = DummyTimValueStack
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
      ValueStack : TimValueStack
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

let initialArgStack = []
let initialValueStack = DummyTimValueStack
let initialDump = DummyTimDump
let intCode = []

let compiledPrimitives = []

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
      ValueStack = initialValueStack
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

let step state =
    let tail = List.tail state.Instrs
    match state.Instrs with
    | Take n :: tail ->
        if List.length state.Stack >= n then
            let heap', fptr' = frameAlloc state.Heap (List.take n state.Stack)
            let stack = List.skip n state.Stack
            { state with
                Instrs = tail
                Fptr = fptr'
                Stack = stack
                Heap = heap'
                Stats = statIncAllocations state.Stats n }
        else
            failwith "Too few args for Take instruction"
    | [ Enter am ] ->
        let code, fptr = amToClosure am state.Fptr state.Heap state.CStore
        { state with
            Instrs = code
            Fptr = fptr }
    | Push am :: tail ->
        { state with 
            Instrs = tail
            Stack = amToClosure am state.Fptr state.Heap state.CStore :: state.Stack }
    | _ -> failwith "Unmatched case for step"

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

and showInstruction d am =
    match am with
    | Take n -> sprintf "%A" am |> iStr
    | Enter am -> iStr "Enter " |>iAppend<| showArg d am
    | Push am -> iStr "Push " |>iAppend<| showArg d am

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
let showValueStack vstack = iNil

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
        showValueStack state.ValueStack;
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