// Parsing

type ComplexOp = And | Or | Imp | Iff

type Formula =
    | True
    | False
    | Symbol of string
    | Not of Formula
    | Complex of ComplexOp * Formula list
    | Forall of Formula * Formula
    | Exists of Formula * Formula
    | Pred of string * Formula list

type Parser<'r> = (char list -> ('r * char list) list)

let isComplex = function
    | Complex _ -> true
    | _ -> false

let inline (!+) (str:string) = str.ToCharArray() |> List.ofArray
let inline (~&) (str:string) = str.ToCharArray() |> List.ofArray
let (~%) (chars:char list) = new System.String(Array.ofList chars)

let shouldParen current parent =
    match current, parent with
    | _, Not _ when isComplex current -> true
    | Complex (cOp,_), Complex (pOp, _) ->
        match cOp, pOp with
        | Or, And -> true
        | Imp, And | Imp, Or -> true
        | Iff, And | Iff, Or | Iff, Imp -> true
        | _ -> false
    | _ -> false

let rec toString fm =
    let acc = new System.Text.StringBuilder()
    let add (s : string) = ignore(acc.Append(s))
    let complexOpToStr = function
        | And -> "&"    
        | Or  ->  "|" 
        | Imp  ->  "=>" 
        | Iff  ->  "<=>" 
    let rec go fm paren  =
        if paren then ignore (acc.Append("("))
        match fm with
        | True -> add "true"
        | False -> add "false"
        | Symbol s -> add s
        | Not fm1 -> 
            add "~"
            go fm1 (shouldParen fm1 fm)
        | Complex (op, fms) ->
            goComplex (complexOpToStr op) fms
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

let printFm name fm = printfn "%s: %A" name (toString fm)
let printAndReturn name fm = printfn "%s: %A" name (toString fm) ; fm


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
    pChain2 (pPrim pExpr) (pAlternatives pOps) (fun fms -> Complex (construct, fms))

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
    | Complex (_, fms) -> List.fold (fun s x -> overSymbols f x s) b fms
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
    | Complex (And, fms) -> List.forall (fun fm -> evalTruth fm env) fms
    | Complex (Or, fms) -> List.exists (fun fm -> evalTruth fm env) fms
    | Complex (Imp, (fm1::fm2::_)) -> not (evalTruth fm1 env) || evalTruth fm2 env
    | Complex (Iff, (fm1::fm2::_)) -> evalTruth fm1 env = evalTruth fm2 env
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
    let mutable fms = []
    for fm1 in kb do
        fms <- fm1::fms
    Complex (And, fms)

let tell (kb : Kb) s = 
    kb.Add(logic s)

let ask (kb : Kb) query =
    validity (Complex (Imp, [kbToFormula kb; logic query])) = Valid

let rec transform f fm = 
    let g = f >> transform f
    match fm with
    | Not fm -> g fm |> Not
    | Complex (op, fms) -> Complex (op, List.map g fms)
    | True | False | Symbol _ -> f fm
    | _ -> f fm

let rec children fm =
    match fm with
    | Complex (_, fms) -> fms
    | Not fms -> [fms]
    | _ -> []

let rec transformChildren f fm =
    match fm with
    | Complex (op, fms) -> Complex (op, List.map f fms)
    | Not fm -> f fm |> Not
    | _ -> fm

let rec eliminateImplications fm =
    let chn = children fm |> List.map eliminateImplications
    match fm with
    | Complex (Imp, _) ->
        let a = List.item 0 chn
        let b = List.item 1 chn
        Complex (Or, [Not a; b])
    | Complex (Iff, _) ->
        let a = List.item 0 chn
        let b = List.item 1 chn
        Complex (And, [Complex (Or, [Not a; b]); Complex (Or, [Not b; a])])
    | Complex (op, _) -> Complex (op, chn)
    | Not fm1 -> eliminateImplications fm1 |> Not
    | _ -> fm
    |> printAndReturn "eliminate"

let rec moveNotInwards fm =
    printFm "->moveNotInwards" fm
    let flipNot = function
        | (Not a) -> a
        | a -> Not a
    match fm with
    | Not (Not a) -> moveNotInwards a
    | Not (Complex (And, fms)) ->
        let fms = List.map (flipNot >> moveNotInwards) fms
        Complex (Or, fms)
    | Not (Complex (Or, fms)) ->
        let fms = List.map (flipNot >> moveNotInwards) fms
        Complex (And, fms)
    | _ -> transformChildren moveNotInwards fm
    |> printAndReturn "<-moveNotInwards"

let combinations orClauses =
    let folder acc el =
        [ for a in acc do
            match el with
            | Complex (And, fms) -> 
                for b in fms do
                   yield (b::a)
            | _ -> yield (el::a) ]
    let listToFormula = function
        | [a] -> a
        | x::xs -> Complex (Or, (x::xs))
        | _ -> failwith "combinations: cannot be empty list"
    List.fold folder [[]] orClauses
    |> List.map (List.rev >> listToFormula)

let rec distributeAndOverOr fm =
    printFm "->distribute" fm
    match fm with
    | Complex(Or, fms) ->
        let fms = List.map distributeAndOverOr fms
        let combs = combinations fms
        if combs.Length > 1 then Complex (And, combs)
        else combs.Head
    | _ -> transformChildren distributeAndOverOr fm
    |> printAndReturn "<-distribute"

let rec unwrap fm =
    printFm "unwrap" fm
    match fm with
    | Complex (op, fms) ->
        let fms = List.map unwrap fms
        let mapping fm1 =
            match fm1 with
            | Complex (op1, fms1) when op = op1 -> fms1
            | _ -> [fm1]
        Complex (op, List.collect mapping fms)
    | Not fm -> unwrap fm |> Not
    | _ -> fm
    |> printAndReturn "unwrap"

let formulaToCnf fm =
    fm
    |> eliminateImplications
    |> moveNotInwards
    |> distributeAndOverOr
    |> unwrap

let toCnf s =
    logic s
    |> eliminateImplications
    |> moveNotInwards
    |> distributeAndOverOr
    |> unwrap

type Literal = { Name : string; IsComplement : bool }

type Clause = Set<Literal>

type Clauses = Set<Clause>

let toLiteral = function
    | Symbol s -> 
        { Name = s; IsComplement = false }
    | Not (Symbol s) -> 
        { Name = s; IsComplement = true }
    | _ -> failwith "Literal is expected"
let toClause fm = 
    match fm with
    | Complex (Or, fms) ->
        List.map toLiteral fms
        |> Set.ofList
    | Symbol _ | Not (Symbol _) -> 
        toLiteral fm |> Set.singleton
    | _ -> failwith "Clause expected"
let cnfToClauses fm : Clauses =
    match fm with
    | Complex (And, fms) ->
        List.map toClause fms
        |> Set.ofList
    | _ -> failwith "CNF formula is expected"

let isEqual a b =
    Complex (Iff, [a; b]) |> validity = Valid

let removeElem xs x =
    List.filter ((<>) x) xs

let plResolve c1 c2 =
    [ for e1 in c1 do
        for e2 in c2 do
            if isEqual (Not e1) e2 then
                yield removeElem c1 e1 
                |>List.append<| 
                removeElem c2 e2 
                |> List.distinct ]

let isComplement (l1 : Literal) (l2 : Literal) =
    l1.Name = l2.Name && l1.IsComplement <> l2.IsComplement
    |> (fun x -> printfn "isComplement: %A %A -> %A" l1 l2 x ; x)

let mergeClauses (c1 : Clause) (l1 : Literal) c2 l2 =
    let c1 = Set.remove l1 c1 |> (fun x -> printfn "mergeClauses1: %A" x; x)
    let c2 = Set.remove l2 c2 |> (fun x -> printfn "mergeClauses2: %A" x; x)

    Set.union c1 c2
    |> (fun x -> printfn "mergeClauses: %A" x; x)

let plResolve2 c1 c2 : Clauses =
    seq {
        for l1 in c1 do
            for l2 in c2 do
                if isComplement l1 l2 then
                    yield mergeClauses c1 l1 c2 l2
    }
    |> Set.ofSeq

let clausesCombinations (clauses : Clauses) =
    [ for c1 in clauses do
        for c2 in clauses.Remove(c1) do
            yield (c1, c2) ]

let isEntailedCnf kb a =
    let kb = kbToFormula kb
    Complex (And, [kb; Not a]) |> formulaToCnf |> cnfToClauses


let plResolution kb a =
    let rec forClauses clausesComb (newClauses : Clauses) oldClauses =
        match clausesComb with
        | (c1, c2) :: rest ->
            let resolvents = plResolve2 c1 c2
            if resolvents.IsEmpty then true
            else 
                let newClauses = Set.union newClauses resolvents
                forClauses rest newClauses oldClauses               
        | _ ->
            if Set.isSubset newClauses oldClauses then false
            else
                let oldClauses = Set.union newClauses oldClauses
                let clausesComb = clausesCombinations oldClauses
                forClauses clausesComb newClauses oldClauses
            
    let clauses = isEntailedCnf kb a
    let clausesComb = clausesCombinations clauses

    forClauses clausesComb Set.empty clauses

let isClauseTrue model clause  =
    let isGoodLiteral (literal : Literal) =
        match List.tryFind (fun (n, _) -> literal.Name = n) model with
        | Some (_, v) -> literal.IsComplement <> v // literal and model value are the same
        | _ -> false

    Set.forall isGoodLiteral clause

let findPureSymbol symbols clauses =
    let mergedClauses = Set.unionMany clauses

    let pureSymbolValueMaybe s =
        let filtered = Seq.filter (fun (l : Literal) -> l.Name = s) mergedClauses
        if Seq.length filtered = 1 then 
            let value = not (Seq.head filtered).IsComplement
            Some (s, value)
        else None

    List.tryPick pureSymbolValueMaybe symbols

let literalToSymbolValue (l : Literal) = l.Name, not l.IsComplement

let findUnitClause clauses model =
    let isNotContracted (l : Literal) =
        List.forall (fun (n, v) -> n <> l.Name || v <> l.IsComplement) model
        |> (fun x -> printfn "isNotContr: %A %A" l x; x)
    let findUnitSymbolValue clause =
        let filtered = Set.filter isNotContracted clause
        if filtered.Count = 1 then
            Seq.head filtered |> literalToSymbolValue |> Some
        else None
        |> (fun x -> printfn "findUnitSymbolValue: %A " x; x)
    
    Seq.tryPick findUnitSymbolValue clauses

let rec DPLL clauses symbols model =
    let updateModel (p, value) =
        DPLL clauses (List.filter ((<>) p) symbols) ((p, value)::model)

    if Set.forall (isClauseTrue model) clauses then true
    elif Set.exists (isClauseTrue model >> not) clauses then false
    else
        match findPureSymbol symbols clauses with
        | Some pvalue ->
            updateModel pvalue
        | _ ->
            match findUnitClause clauses model with
            | Some pvalue ->
                updateModel pvalue
            | _ ->
                let p = List.head symbols
                updateModel (p, true) || updateModel (p, false)

let clausesToSymbols clauses =
    Seq.collect (fun clause ->
        Seq.map (fun (l : Literal) -> l.Name) clause
        |> Seq.distinct) clauses
    |> List.ofSeq
    |> List.distinct

let isEntailedDPLL kb a =
    let clauses = isEntailedCnf kb a
    let symbols = clausesToSymbols clauses
    DPLL clauses symbols []

///////////////////////////////////////////////////////////
/// Testing
///////////////////////////////////////////////////////////
let vvv = function
    | (Complex (Or, fms)) -> fms
    | a -> [a]
plResolve (vvv (logic "~P21|B11")) (vvv (logic ("~B11|P12|P21"))) |> List.map (List.map toString)
plResolve (vvv (logic "P11|P31")) (vvv (logic ("~P11|P22"))) |> List.map (List.map toString)
plResolve (vvv (logic "P11")) (vvv (logic ("~P11"))) |> List.map (List.map toString)

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

let testIt given expected =
    let v = given |> toCnf |> toString
    if not (v = expected) then
        failwithf "Expected: \n\t'%s'\nbut was\n\t'%s'" expected v


let testCnf () =
    testIt "~(A & B)" "~A | ~B"
    testIt "~~~~A" "A"
    testIt "~(((~P | ~Q) => ~(P | Q)) => R)" "(P | ~P) & (P | ~Q) & (Q | ~P) & (Q | ~Q) & ~R"
    testIt "(((~P | ~Q) => ~(P | Q)) => R)" "(~P | ~Q | R) & (P | Q | R)"
    testIt "(A | (B | (C & D))) | E | (F | (G | (H & I)))" "(A | B | C | E | F | G | H) & (A | B | C | E | F | G | I) & (A | B | D | E | F | G | H) & (A | B | D | E | F | G | I)"
    testIt "A & B & C & D & (E | (F & G)) & H & I & J & K" ""

"(B11 <=> (P12 | P21)) & ~B11"  |> toCnf |> cnfToClauses
"(B11 <=> (P12 | P21)) & ~B11"  |> toCnf|> toString
plResolve2 (toCnf "~P21|B11" |> toClause ) (toCnf "~B11|P12|P21" |> toClause)

let testResolution () =
    let kb = makeKb ()
    tell kb ("B12 <=> P11 | P13 | P22 | P02")
    tell kb ("B21 <=> P20 | P22 | P31 | P11")
    tell kb ("B01 <=> P00 | P02 | P11")
    tell kb ("B10 <=> P11 | P20 | P00")
    tell kb ("~B21")
    tell kb ("~B12")
    tell kb ("B10")
    tell kb ("B01")
    plResolution kb (logic "P00")

let testResolution2 () =
    let kb = makeKb ()
    tell kb ("(B11 <=> P12 | P21) & ~B11")
    plResolution kb (logic "B")

let testDPLLSimple () =
    let fm = logic "A&B&(A|B)"
    let fm = logic "A&B"
    let clauses = fm |> formulaToCnf |> cnfToClauses
    let symbols = clausesToSymbols clauses

    DPLL clauses symbols []
    
let testDPLL () =
    let kb = makeKb ()
    tell kb ("B12 <=> P11 | P13 | P22 | P02")
    tell kb ("B21 <=> P20 | P22 | P31 | P11")
    tell kb ("B01 <=> P00 | P02 | P11")
    tell kb ("B10 <=> P11 | P20 | P00")
    tell kb ("~B21")
    tell kb ("~B12")
    tell kb ("B10")
    tell kb ("B01")
    isEntailedDPLL kb (logic "P00")

findUnitClause (Set.singleton 
    (Set.ofList 
        [{Name = "A"; IsComplement = false}; 
        {Name="B"; IsComplement = true}])) ["A", false]

testDPLLSimple();;