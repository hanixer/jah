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

type PushType = 
    | PArg
    | PCont

type TimAMode =
    | Arg of int
    | Label of string
    | Code of Instruction list
    | IntConst of int
    | Data of int
and Instruction =
    | Take of int * int
    | Move of int * TimAMode
    | Enter of TimAMode
    | Push of PushType * TimAMode
    | PushV of ValueAMode
    | Op of Op
    | Return
    | Cond of (Instruction list * Instruction list)
    | PushMarker of int
    | UpdateMarkers of int
    | Switch of (int * Instruction list) list
    | ReturnConstr of int
    | Print

type Closure = (Instruction list * FramePtr)
type TimStack = Closure list

type TimValueStack = int list
type TimDumpItem = 
    { Fptr : FramePtr
      ClosNum : int
      ArgStack : TimStack
      ContStack : TimStack }
type TimDump = TimDumpItem list

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
      Dptr : FramePtr
      ArgStack : TimStack
      ContStack : TimStack
      VStack : TimValueStack
      Dump : TimDump
      Heap : TimHeap
      CStore : CodeStore
      Stats : TimStats
      Output : (string * bool)
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
let initialValueStack = []
let initialDump = []
let intCode = [PushV FramePtr; Return]

let mkEnter = function
    | Code i -> i
    | am -> [Enter am]

let compiledPrimitives = 
    [  ]

let arithmeticOps = 
    [ "+", Add
      "-", Sub
      "*", Mul
      "/", Div
      "<", Lt ]

let isArithmeticOp op = List.map fst arithmeticOps  |> List.contains op

let isBCompilable = function
    | EAp (EAp (EVar op, _), _) -> isArithmeticOp op
    | ENum _ -> true
    | _ -> false

let opToOp op = 
    match Util.listTryFindFirst op arithmeticOps with
    | Some (_, x) ->  x
    | _ -> failwithf "Operation %s not found" op

let mkIndNode n = Code (mkEnter (Arg n))

let mkUpdIndNode n = Code [ PushMarker n; Enter (Arg n) ]

let isACompilable = function
    | EVar _ | ENum _ -> true
    | _ -> false

let rec compileA expr env =
    match expr with
    | ENum n -> IntConst n
    | EVar v -> 
        match listTryFindFirst v env with
        | Some (_, x) -> x
        | None -> failwithf "Unknown variable '%s'" v
    | _ -> failwith "Only integer or variable is expected for A scheme"

and processBinding env (d, ams, u) (v, e) =
    let d', am = compileU e u env d
    d', (am :: ams), u + 1

and compileR expr env d =
    match expr with
    | _ when isBCompilable expr ->
        compileB expr env d [Return]
    | _ when isACompilable expr -> 
        let am = compileA expr env
        d, (mkEnter am)
    | EAp (e1, e2) when isACompilable e2 ->
        let d1, instrs = compileR e1 env d
        let am = compileA e2 env
        d1, (Push (PArg, am)) :: instrs
    | EAp (e1, e2) ->
        let d1, am = compileU e2 (d + 1) env (d + 1)
        let d2, instrs = compileR e1 env d1
        let move = Move (d + 1, am)
        let pushArg = Push (PArg, Code [Enter (Arg (d + 1))])
        let allInstrs = move :: pushArg :: instrs
        d2, allInstrs
    | ELet (false, bindings, body) ->
        let n = List.length bindings
        let dn, ams, _ = List.fold (processBinding env) (d + n, [], d + 1) bindings
        let ams = List.rev ams
        let bindingsEnv = List.mapi (fun i (v, _) -> v, mkUpdIndNode (d + i + 1)) bindings
        let bodyEnv = List.append bindingsEnv env
        let d', bodyInstrs = compileR body bodyEnv dn
        let moves = List.mapi (fun i am -> Move (d + i + 1, am)) ams
        d', List.append moves bodyInstrs
    | ELet (true, bindings, body) ->
        let n = List.length bindings
        let bindingsEnv = List.mapi (fun i (v, _) -> v, mkIndNode (d + i + 1)) bindings
        let bodyEnv = List.append bindingsEnv env
        let dn, ams, _ = List.fold (processBinding bodyEnv) (d + n, [], d + 1) bindings
        let ams = List.rev ams
        let d', bodyInstrs = compileR body bodyEnv dn
        let moves = List.mapi (fun i am -> Move (d + i + 1, am)) ams
        d', List.append moves bodyInstrs
    | EConstr (tag, 0) ->
        d, [ReturnConstr tag]
    | EConstr (tag, arity) ->
        d, [ UpdateMarkers arity; Take (arity, arity); ReturnConstr tag ]
    | ECase (e, alters) ->
        let handleAlter dAcc alter =
            let dNew, res = compileE alter env d
            max dAcc dNew, res
        let d1, compiledAlters = mapAccumul handleAlter d alters
        let d2, compiledE = compileR e env d1
        d2, Push (PCont, (Code [Switch compiledAlters])) :: compiledE
    | _ -> failwith "compileR cannot compile this yet"

and compileE (tag, vars, body) env d =
    let n = List.length vars
    let moves = [ for i = 1 to n do yield Move (d + i, Data i) ]
    let bodyEnv = 
        List.mapi (fun i v -> v, Arg (i + 1)) vars
        |> List.append <| env
    let d1, bodyInstrs = compileR body bodyEnv (d + n)
    d1, (tag, List.append moves bodyInstrs)

and compileB expr env d cont =
    match expr with
    | EAp (EAp (EVar opVar, e1), e2) when isArithmeticOp opVar ->
        let cont1 = Op (opToOp opVar) :: cont
        let d1, cont2 = compileB e1 env d cont1
        let d2, instrs = compileB e2 env d cont2
        max d1 d2, instrs
    | ENum n ->
        d, (PushV (IntVConst n) :: cont)
    | _ ->
        let d1, compiled = compileR expr env d
        d1, (Push (PCont, Code cont) :: compiled)

and compileU expr u env d =
    let d1, instrs = compileR expr env d
    d1, Code (PushMarker u :: instrs)

let extendEnvWithArgs env args : TimCompilerEnv =
    args 
    |> List.mapi (fun i arg -> arg, mkUpdIndNode (i + 1))
    |> List.append <| env

let compileSc env (name, args, body)  : Name * (Instruction list) =
    let n = List.length args
    let newEnv = extendEnvWithArgs env args
    let d, compiledBody = 
        compileR body newEnv n
    let instrs =
        if n = 0 && d = 0 then compiledBody
        else UpdateMarkers n :: Take (d, n) :: compiledBody
    name, instrs

let extraPreludeDefs =
    [ "cons", [], EConstr (2, 2)
      "nil", [], EConstr (1, 0)
      "true", [], EConstr (2, 0)
      "false", [], EConstr (1, 0)
      "if", ["cond"; "tbranch"; "fbranch"], (ECase (EVar "cond", [1, [], EVar "fbranch"; 2, [], EVar "tbranch"])) ]

let topCont = 
    let headCont =
        [ Print; Push (PCont, Label "topCont"); Enter (Arg 2) ]
    Switch 
        [ 1, []
          2, [ Move (1, (Data 1))
               Move (2, Data 2)
               Push (PCont, Code headCont)
               Enter (Arg 1) ] ]
    |> List.singleton

let compile program =
    let scDefs = List.concat [preludeDefs; extraPreludeDefs; program]
    let initialEnv : TimCompilerEnv = 
        [ for name, args, body in scDefs do yield name, Label name ]
        |> List.append <|
        [ for name, code in compiledPrimitives do yield name, Label name ]
    let compiledScDefs = List.map (compileSc initialEnv) scDefs
    let compiledCode =
        [ compiledScDefs
          ["topCont", topCont]
          compiledPrimitives ]
        |> List.concat
    let heap, frame = frameAlloc heapEmpty (List.replicate 2 ([], FrameNull))
    { Instrs = mkEnter (Label "main")
      Fptr = FrameNull 
      Dptr = FrameNull
      ArgStack = initialArgStack
      ContStack = [topCont, frame]
      VStack = initialValueStack
      Dump = initialDump
      Heap = heap
      CStore = compiledCode
      Exception = None
      Output = ("", false)
      Stats = statInitial }

let timFinal state = state.Instrs.IsEmpty

let amToClosure am state : Closure =
    match am with
    | Arg n -> frameGet state.Heap state.Fptr n
    | Code il -> il, state.Fptr
    | Label l -> codeLookup state.CStore l, state.Fptr
    | IntConst n -> intCode, FrameInt n
    | Data n -> frameGet state.Heap state.Dptr n

let take t n state =
    if List.length state.ArgStack < n then
        failwith "Too few args for Take instruction"
    let fromStack = List.take n state.ArgStack
    let empty = [], FrameNull
    let frame = Seq.replicate (t - n) empty |> List.ofSeq
    let heap', fptr' = 
        List.append fromStack frame
        |> frameAlloc state.Heap
    let stack = List.skip n state.ArgStack
    { state with
        Fptr = fptr'
        ArgStack = stack
        Heap = heap'
        Stats = statIncAllocations state.Stats n }

let move i am state =
    let closure = amToClosure am state
    let heap = frameUpdate state.Heap state.Fptr i closure
    { state with Heap = heap }

let enter am state =
    if not state.Instrs.IsEmpty then 
        failwith "Enter must be last instruction"
    let code, fptr = amToClosure am state
    { state with
        Instrs = code
        Fptr = fptr }

let push pushType am state =
    let clos = amToClosure am state
    match pushType with
    | PArg -> { state with ArgStack = clos :: state.ArgStack }
    | PCont -> { state with ContStack = clos :: state.ContStack }

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
        | Lt -> fun x y -> if x < y then 2 else 1
        | _-> (+)
    match state.VStack with
    | n1 :: n2 :: vstack ->
        { state with VStack = (f n1 n2) :: vstack }
    | _ -> failwith "Expected two numbers on VStack"

let cond i1 i2 state =
    match state.VStack with
    | 0 :: vstack ->
        { state with Instrs = i1; VStack = vstack }
    | n :: vstack ->
        { state with Instrs = i2; VStack = vstack }
    | _ -> failwith "A number is expected for Cond instruction on VStack"

let pushMarker n state =
    let d = { Fptr = state.Fptr; ClosNum = n; ContStack = state.ContStack; ArgStack = state.ArgStack }
    { state with
        Dump = d :: state.Dump
        ContStack = []
        ArgStack = [] }

let updateMarkersInstructions m n i =
    [ [ for i = m downto 1 do yield Push (PArg, Arg i) ]
      [ UpdateMarkers n ]
      i ]
    |> List.concat

let updateMarkers n state =
    let m = List.length state.ArgStack
    if m >= n then state
    else
        let heap1, fptr = frameAlloc state.Heap state.ArgStack
        match state.Dump with
        | d :: dump ->
            let clos = (updateMarkersInstructions m n state.Instrs, fptr)
            let heap2 = frameUpdate heap1 d.Fptr d.ClosNum clos
            { state with
                Instrs = (UpdateMarkers n) :: state.Instrs
                Heap = heap2
                ArgStack = List.append state.ArgStack d.ArgStack
                ContStack = d.ContStack
                Dump = dump }
        | _ -> failwith "Non-empty dump is expected in updateMarkers"

let switch jumps state =
    if not state.Instrs.IsEmpty then failwith "Switch instruction must be last"

    let tag = List.head state.VStack
    match Util.listTryFindFirst tag jumps with
    | Some (_, instrs) ->
        { state with
            Instrs = instrs
            VStack = List.tail state.VStack }
    | None -> failwith "No matching tag is found for Switch instruction."

let returnInstr state =
    if not state.Instrs.IsEmpty then 
        failwith "Return must be last instruction"

    match state.ContStack, state.Dump with
    | (i, f) :: stack, _ ->
        { state with
            Instrs = i
            Fptr = f
            ContStack = stack }
    | [], d :: dump ->
        match state.VStack with
        | n :: vstack ->        
            let heap = frameUpdate state.Heap d.Fptr d.ClosNum (intCode, FrameInt n) 
            { state with
                Heap = heap
                ContStack = d.ContStack
                ArgStack = d.ArgStack
                Dump = dump
                Instrs = [Return] }
        | [] -> failwith "VStack must be non-empty for return instruction"
    | _ -> failwith "Stack must be non-empty for return instruction"

let returnConstr tag state =
    if not state.Instrs.IsEmpty then failwith "ReturnConstr instruction must be last"

    match state.ContStack, state.Dump with
    | (instrs, fptr) :: contStack, _ ->
        { state with
            VStack = tag :: state.VStack
            ContStack = contStack
            Dptr = state.Fptr
            Instrs = instrs
            Fptr = fptr }
    | [], d :: dump ->
        let heap = frameUpdate state.Heap d.Fptr d.ClosNum ([ReturnConstr tag], state.Fptr)
        { state with
            Heap = heap
            ContStack = d.ContStack
            ArgStack = d.ArgStack
            Dump = dump
            Instrs = [ReturnConstr tag] }
    | _ -> failwith "Both ContStack and Dump are empty during ReturnConstr"

let print state =
    let output, isChanged = state.Output
    { state with Output = (string state.VStack.[0]), true }

let dispatch = function
    | Take (t, n) -> take t n
    | Move (i, am) -> move i am
    | Enter am -> enter am
    | Push (pushType, am) -> push pushType am
    | PushV vm -> pushv vm
    | Op op -> oper op
    | Return -> returnInstr
    | Cond (i1, i2) -> cond i1 i2
    | PushMarker n -> pushMarker n
    | UpdateMarkers n -> updateMarkers n
    | Switch jumps -> switch jumps
    | ReturnConstr tag -> returnConstr tag
    | Print -> print

let step state =
    let tail = List.tail state.Instrs
    match state.Instrs with
    | instr :: instrs ->
        dispatch instr { state with Instrs = instrs; Output = fst state.Output, false }
    | _ -> failwith "Execution must be finished at this point"

let doAdmin s =
    { s with 
        Stats = statIncSteps s.Stats}

exception OverflowException

let rec eval state =
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
    | Push (pt, am) -> iStr "Push " |>iAppend<| iStr (sprintf "%A " pt) |>iAppend<| showArg d am
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
    | Switch items ->
        let showItem (tag, il) =
            [ iStr "<"; iNum tag; iStr ">"; iStr " -> ";
              showInstructions d il |> iIndent;
              iNewline ]
            |> iConcat
        let items = List.sortBy fst items
        [ iStr "Switch ";
          List.map showItem items |> iConcat |> iIndent ]
        |> iConcat
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

let showDumpItem (d: TimDumpItem) =
    [ iStr "(";
      showFramePtr d.Fptr;
      iStr ", ";
      iNum d.ClosNum;
      iStr ")"; ]
    |> iConcat

let showDump dump =
    [ iStr "Dump:";
      iNewline;
      iStr "  [";
      List.map showDumpItem dump |> iInterleave iNewline |> iIndent;
      iStr "]" ]
    |> iConcat

let showValueStack vstack =
    iConcat 
        [ iStr "VStack:["
          List.map iNum vstack |> iInterleave (iStr ", ")
          iStr "]"
          iNewline ]

let showStack pushType stack =
    let s = 
        match pushType with
        | PArg -> "Arg stack:"
        | PCont -> "Cont stack:"
    iConcat
        [ iStr s;
          iNewline;
          iStr "  ["
          List.map showClosure stack |> iInterleave iNewline |> IIdent;
          iStr "]"; iNewline ]

let showFrame frameType heap f =
    match f with
    | FrameNull -> sprintf "Null %s" frameType |> iStr |>iAppend<| iNewline
    | FrameAddr a ->
        iConcat
            [ sprintf "%s (addr " frameType |> iStr;
              iFWNum 2 a;
              iStr "):";
              iNewline;
              iStr "  <";              
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
    let code = [ iStr "Code:  "; showInstructions Terse state.Instrs ] |> iConcat
    [
        code;
        showFrame "Fptr" state.Heap state.Fptr;
        showFrame "Dptr" state.Heap state.Dptr;
        showStack PArg state.ArgStack;
        showStack PCont state.ContStack;
        showValueStack state.VStack;
        showDump state.Dump;
        showException state.Exception;
    ]
    |> iInterleave iNewline

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
        
let showOutput states =
    let show i state =
        let i = i + 1
        let x =
            if state.Output |> snd then fst state.Output |> iStr
            else iStr "."
        if i % 50 = 0 then iAppend x iNewline
        else x
    [ iStr "Output"; iNewline;
      List.mapi show states |> iConcat ]
    |> iConcat

let showFullResults states =
    let s  = List.head states
    let last = List.last states
    iConcat
        [ iStr "Supercombinator definitions"; iNewline;
          showScDefs s; iNewline; iNewline;
          iStr "State transitions"; iNewline; iNewline;
          iLayn (List.map showState states); iNewline; iNewline;
          showStats (List.last states); iNewline; iNewline;
          showOutput states; ]
    |> iDisplay

let showResults states =
    [ showState (List.last states); iNewline; iNewline; 
      showStats (List.last states) ]
    |> iConcat
    |> iDisplay

let runProg<'p> = parse >> compile >> eval >> showResults

let fullRun<'p> = parse >> compile >> eval >> showFullResults