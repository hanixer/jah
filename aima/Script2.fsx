// Parsing


type Formula =
    | True
    | False
    | Symbol of string
    | Not of Formula
    | And of Formula list
    | Or of Formula list
    | Imp of Formula list
    | Iff of Formula list
    | Forall of Formula * Formula
    | Exists of Formula * Formula
    | Pred of string * Formula list

type Parser<'r> = (char list -> ('r * char list) list)

let isComplex = function
    |  And _ | Or _ | Imp _ | Iff _ -> true
    | _ -> false

let inline (!+) (str:string) = str.ToCharArray() |> List.ofArray
let inline (~&) (str:string) = str.ToCharArray() |> List.ofArray
let (~%) (chars:char list) = new System.String(Array.ofList chars)

let shouldParen current parent =
    match current, parent with
    | _, Not _ when isComplex current -> true
    | Or _, And _ -> true
    | Imp _, And _ | Imp _, Or _ -> true
    | Iff _, And _ | Iff _, Or _ | Iff _, Imp _ -> true
    | _ -> false

let rec toString fm =
    printfn "toString:"
    let acc = new System.Text.StringBuilder()
    printfn "%A" acc
    let add (s : string) = ignore(acc.Append(s))

    printfn "%A" acc
    let rec go fm paren  =
        printfn "go: %A %A" fm paren
        if paren then ignore (acc.Append("("))
        match fm with
        | True -> add "true"
        | False -> add "false"
        | Symbol s -> add s
        | Not fm1 -> 
            add "~"
            go fm1 (shouldParen fm1 fm)
        | And fms-> goComplex "&" fms
        | Or fms -> goComplex "|" fms
        | Imp fms -> goComplex "=>" fms
        | Iff fms -> goComplex "<=>" fms
        | _ -> ()
        if paren then add(")")

    and goComplex op fms =
        match  fms with
        | fm1 :: fms ->
            go fm1 (shouldParen fm1 fm)
            List.iter (fun fm2 ->
                add (" " + op + " ")
                go fm2 (shouldParen fm2 fm)) fms
        | _ -> ()

    go fm false
    acc.ToString()

toString (Not (And [Not True; False]))

(*
let rec toString fm =
    let rec go current parent 
        let shouldParen =
            match current, parent with
            | And _, Some (And _) -> false
            | And _, Some (Not _) -> false
            | And _, Some p when isComplex p -> true
*)
let parse p = p

let parsestr p str = parse p [for c in str -> c]

let (>>=) p f =  (fun cs ->
    List.concat [for (r, cs') in parse p cs -> parse (f r) cs'])

let (>>.) p q = p >>= (fun _ -> q)

let mreturn r =  (fun cs -> [(r,cs)])

let empty =  (fun _ -> [])

let item =  (fun cs ->
    match cs with
    | [] -> []
    | c :: cs' -> [(c,cs')])

let sat cond =
    item >>= (fun c -> if cond c then mreturn c else empty)

let charp c = sat ((=) c)

let digit = sat (fun c ->
    (List.tryFind ((=)c) ['0'..'9']).IsSome)

let alpha = sat (fun c ->
  (List.tryFind ((=)c)(List.append
    ['a'..'z'] ['A'..'Z'])).IsSome)

let (<|>) p q = (fun cs ->
    match parse p cs with
    | [] -> parse q cs
    | rs -> rs)

let (++) p q = (fun cs ->
    List.append (parse p cs) (parse q cs))

let rec many0 p = many1 p <|> mreturn []
and many1 p = p >>= fun r -> many0 p >>= fun rs -> mreturn (r::rs)

let symbol =
    let f = (~%) >> Symbol >> mreturn
    alpha >>= fun a ->
    (many1 (alpha <|> digit) >>= fun other -> f (a::other)) <|> f [a] 

let rec symbolLiteral (s : char seq) =
    let s = List.ofSeq s 
    match s with
    | [] -> mreturn []
    | c :: cs -> charp c >>. symbolLiteral cs >>. mreturn s

let pSkipWs = many0 (sat (System.Char.IsWhiteSpace))

let pToken p = p >>= fun r -> pSkipWs >>. mreturn r

let pLiteral s = symbolLiteral s |> pToken

let pSymbol = symbol |> pToken

let pChainl p op = 
    let rec rest x = 
        (op >>= fun opr ->
         p >>= fun y ->
         rest (opr (x, y))) <|> mreturn x
    p >>= rest

let pChain2 pPrim pOp construct =
    pPrim >>= fun x ->
    many0 (pOp >>. pPrim) >>= fun xs ->
    let res = if xs.IsEmpty then x else x :: xs |> construct
    mreturn res

let rec pPrim pExpr =
    (pLiteral "true" >>. mreturn True ) <|>
    (pLiteral "false" >>. mreturn False) <|>
    (pLiteral "~" <|> pLiteral "not" >>. (fun r -> pPrim pExpr r) >>= (Not >> mreturn)) <|>
    pSymbol <|>
    (pLiteral "(" >>. pExpr >>= fun expr ->  pLiteral ")" >>. mreturn expr)

let pAlternatives = function
    | p :: ps ->
        List.fold (<|>) p ps
    | _ -> failwith "At least one parser is needed"

let pBinary pExpr pPrim ops construct =
    let pOps = List.map pLiteral ops
    pChain2 (pPrim pExpr) (pAlternatives pOps) construct

let pAnd pExpr =
    pBinary pExpr pPrim ["&"; "^"; "and"] And

let pOr pExpr =
    pBinary pExpr pAnd ["|"; "or"] Or

let pImp pExpr =
    pBinary pExpr pOr ["=>"] Imp

let pIff pExpr =
    pBinary pExpr pImp ["<=>"] Iff
    
let rec pExpr =
    let parserRef : Parser<Formula> ref= ref (fun _ -> [])
    let parserFunc = (fun cs -> !parserRef cs)
    parserRef := pIff parserFunc
    !parserRef

let logic s = 
    match parsestr pExpr s with
    | (fm, _) :: _ -> fm
    | _ -> failwith "Formula parse error"

// Parser end

let rec overSymbols f a b =
    match a with
    | Symbol a -> f a b
    | Not fm -> overSymbols f fm b
    | And fms | Or fms | Imp fms | Iff fms -> List.fold (fun s x -> overSymbols f x s) b fms
    | _ -> b

let propSymbolsIn fm = overSymbols (fun x xs -> x::xs) fm []

let allTruthInterpretations fm =
    let symbols = propSymbolsIn fm
    let folder truthTables symbol =
        [ for truthTable in truthTables do
            for p in [(symbol, true); (symbol, false)] do
                yield p :: truthTable ]
    List.fold folder [[]] symbols

type Validity = Valid | Satisfiable | Unsatisfiable

let rec evalTruth fm env =
    match fm with
    | True -> true
    | False -> false
    | Not fm -> not (evalTruth fm env)
    | Symbol s -> 
        match List.tryFind ((fst >> ((=) s))) env with
        | Some (_, v) -> v
        | _ -> failwithf "Symbol '%s' not found" s
    | And fms -> List.forall (fun fm -> evalTruth fm env) fms
    | Or fms -> List.exists (fun fm -> evalTruth fm env) fms
    | Imp (fm1::fm2::_) -> not (evalTruth fm1 env) || evalTruth fm2 env
    | Iff (fm1::fm2::_) -> evalTruth fm1 env = evalTruth fm2 env
    | _ -> failwithf "Not implemented: %A" fm

let validity fm = 
    let interpretations = allTruthInterpretations fm
    let fmResults = List.map (evalTruth fm) interpretations
    if List.forall id fmResults then Valid
    else if List.exists id fmResults then Satisfiable
    else Unsatisfiable
let validitys s =
    logic s |> validity

type Kb = System.Collections.Generic.List<Formula>

let makeKb () = new System.Collections.Generic.List<Formula>()
let kbToFormula (kb : Kb) =
    let mutable fm = []
    for fm1 in kb do
        fm <- fm1::fm
    And fm

let tell (kb : Kb) s = 
    kb.Add(logic s)
    printfn "tell: %s === %A" s (kbToFormula kb)

let askEach kb query fn =
    if validity (Imp [kbToFormula kb; logic query]) = Valid then
        fn 1

let ask (kb : Kb) query =
    printfn "ask: %s" query
    validity (Imp [kbToFormula kb; logic query]) = Valid

let rec transform f fm = 
    printfn "transform: %A" fm
    let g = f >> transform f
    match fm with
    | Not fm -> g fm |> Not
    | And fms -> List.map g fms |> And 
    | Or fms -> List.map g fms |> Or
    | Imp fms -> List.map g fms |> Imp
    | Iff fms -> List.map g fms |> Iff
    | True | False | Symbol _ -> f fm
    | _ -> f fm

let rec children fm =
    match fm with
    | And fms | Or fms | Imp fms | Iff fms -> fms
    | Not fms -> [fms]
    | _ -> []

let rec transformChildren f fm =
    match fm with
    | And fms -> List.map f fms |> And
    | Or fms -> List.map f fms |> Or
    | Imp fms -> List.map f fms |> Imp
    | Iff fms -> List.map f fms |> Iff
    | Not fm -> f fm |> Not
    | _ -> fm

let rec eliminateImplications fm =
    let chn = children fm |> List.map eliminateImplications
    match fm with
    | Imp _ -> 
        let a = List.item 0 chn
        let b = List.item 1 chn
        Or [Not a; b]
    | Iff _ ->
        let a = List.item 0 chn
        let b = List.item 1 chn
        And [Or [Not a; b]; Or [Not b; a]]
    | And _ -> And chn
    | Or _ -> Or chn
    | _ -> fm

let rec moveNotInwards fm = 
    let chn = children fm
    match fm with
    | Not (Not a) -> moveNotInwards a
    | Not (And fms) ->
        List.map Not fms |> Or
    | Not (Or fms) ->
        List.map Not fms |> And
    | _ -> transformChildren moveNotInwards fm

let combinations orClauses =
    let folder acc el =
        [ for a in acc do
            match el with
            | And fms -> 
                for b in fms do
                   yield (b::a)
            | _ -> yield (el::a) ]
    let listToFormula = function
        | [a] -> a
        | x::xs -> Or (x::xs)
        | _ -> failwith "combinations: cannot be empty list"
    List.fold folder [[]] orClauses
    |> List.map (List.rev >> listToFormula)

let rec distributeAndOverOr fm =
    printfn "distribute: %A" fm
    match fm with
    | Or fms ->
        let fms = List.map distributeAndOverOr fms
        let combs = combinations fms
        if combs.Length > 1 then And combs
        else combs.Head
    | _ -> transformChildren distributeAndOverOr fm

let rec unwrap fm =
    1

let toCnf s =
    logic s
    |> eliminateImplications
    |> moveNotInwards
    |> distributeAndOverOr
    |> (fun x ->
        printfn "toCnf: %A" x
        x)

let t() = 
    let kb = makeKb()
    ((tell kb "S => W1|W2|W3|W4"))
    ((tell kb "S"))
    ((tell kb "~W1"))
    ((tell kb "~W2"))
    ((ask kb "W4"))
    ((tell kb "~W3"))
    ((ask kb "W4") )
    ((tell kb "Ok <=> ~W ^ ~P"))
    ((tell kb "Ok"))
    ((ask kb "W") )
    ((ask kb "~W") )
    ((tell kb "ToBe and ~ToBe"))
    ((ask kb "SillyQuestion") )

let e = eliminateImplications (logic "b11<=>(p12|p21)")
moveNotInwards e
moveNotInwards (logic "~~~~~~a")

combinations [Symbol "a"; And [Symbol "b"; Symbol "c"]]
distributeAndOverOr (logic "A | B & C")
distributeAndOverOr (logic "~(A & B)")

toString (toCnf "A&(B&(C&D))")
toString(toCnf("A <=> B"))
toString(toCnf("~~A <=> B"))
toString(toCnf("~~~A <=> B"))
toString(toCnf("~~~~A <=> B"))
toString(toCnf("~(A & B)"))
toString(toCnf("~(A | B)"))
toString(toCnf("B11 <=> P12 | P21"))
(*
assert toString(toCnf('A <=> B')) == '((A | ~B) & (B | ~A))'
assert toString(toCnf("B <=> (P1 | P2)")) == '((~P1 | B) & (~P2 | B) & (P1 | P2 | ~B))'
assert toString(toCnf('A <=> (B & C)')) == '((A | ~B | ~C) & (B | ~A) & (C | ~A))'
assert toString(toCnf("a | (b & c) | d")) == '((b | a | d) & (c | a | d))'
assert toString(toCnf("A & (B | (D & E))")) == '(A & (D | B) & (E | B))'
assert toString(toCnf("A | (B | (C | (D & E)))")) == '((D | A | B | C) & (E | A | B | C))'
assert toString(toCnf('(A <=> ~B) ==> (C | ~D)')) == '((B | ~A | C | ~D) & (A | ~A | C | ~D) & (B | ~B | C | ~D) & (A | ~B | C | ~D))'
*)