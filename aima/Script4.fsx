type Term =
    | Fn of string * Term list
    | Const of string
    | Var of string

type ComplexOp = And | Or | Imp | Iff

type Formula =
    | Pred of string * Term list
    | TermEqual of Term * Term
    | Complex of ComplexOp * Formula * Formula
    | Not of Formula
    | Forall of string list * Formula
    | Exists of string list * Formula


type Literal =
    | LTermEqual of bool * Term * Term
    | LPred of bool * string * Term list
type Clause = Set<Literal>
type Clauses = Set<Clause>

let inline (~&) (str:string) = str.ToCharArray() |> List.ofArray
let (~%) (chars:char list) = new System.String(Array.ofList chars)

let isComplex = function
    | Complex _ -> true
    | _ -> false

let shouldParen current parent =
    match current, parent with
    | _, Not _ when isComplex current -> true
    | Complex (cOp,_,_), Complex (pOp, _,_) ->
        match cOp, pOp with
        | Or, And -> true
        | Imp, And | Imp, Or -> true
        | Iff, And | Iff, Or | Iff, Imp -> true
        | _ -> false
    | Exists _, Complex _ | Forall _, Complex _ -> true
    | _ -> false

let rec toString fm =
    let acc = new System.Text.StringBuilder()
    let add (s : string) = ignore(acc.Append(s))
    let printList l recfunc i t1 =
        recfunc t1
        if i <> (List.length l - 1) then
            add ", "
    let complexOpToStr = function
        | And -> "&"    
        | Or  ->  "|" 
        | Imp  ->  "=>" 
        | Iff  ->  "<=>" 
    let rec go fm paren  =
        if paren then ignore (acc.Append("("))
        match fm with
        | Pred (name, terms) ->
            add name
            if terms.Length > 0 then
                add "("
                List.iteri (printList terms goTerm) terms
                add ")"
        | Not fm1 -> 
            add "~"
            go fm1 (shouldParen fm1 fm)
        | Complex (op, fm1, fm2) ->
            goComplex (complexOpToStr op) fm1 fm2 fm
        | Forall (vars, fm1) ->
            add "forall "
            List.iteri (printList vars add) vars
            add " "
            go fm1 (shouldParen fm1 fm)
        | Exists (vars, fm1) ->
            add "exists "
            List.iteri (printList vars add) vars
            add " "
            go fm1 (shouldParen fm1 fm)
        | TermEqual (t1, t2) ->
            goTerm t1
            add " = "
            goTerm t2
        if paren then add(")")

    and goComplex op fm1 fm2 fm =
        go fm1 (shouldParen fm1 fm)
        add (" " + op + " ")
        go fm2 (shouldParen fm2 fm)

    and goTerm t =
        match t with
        | Fn (name, terms) ->
            add name
            add "("
            List.iteri (printList terms goTerm) terms
            add ")"
        | Const name -> add name
        | Var name -> add name

    go fm false
    acc.ToString()

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

let alphaLower = sat (fun c ->
  (List.tryFind ((=)c) ['a'..'z'] ).IsSome)

let alphaUpper = sat (fun c ->
  (List.tryFind ((=)c) ['A'..'Z'] ).IsSome)

let (<|>) p q = (fun cs ->
    match parse p cs with
    | [] -> parse q cs
    | rs -> rs)

let (++) p q = (fun cs ->
    List.append (parse p cs) (parse q cs))

let rec many0 p = many1 p <|> mreturn []
and many1 p = p >>= fun r -> many0 p >>= fun rs -> mreturn (r::rs)

let pThen combiner p1 p2 toks =
    [ for (v1, toks1) in p1 toks do
        for (v2, toks2) in p2 toks1 do
            yield combiner v1 v2, toks2 ]


let pThen3 combiner p1 p2 p3 =
    pThen (fun a b -> a b) (pThen combiner p1 p2) p3

let rec pOneOrMoreWithSep p pSep =
    let pSepAndP = pThen (fun _ x -> x) pSep p
    pThen (fun x xs -> x :: xs) p (many0 pSepAndP)

let pSkipWs = many0 (sat (System.Char.IsWhiteSpace))
let pToken p = p >>= fun r -> pSkipWs >>. mreturn r

let (=?) s1 s2 = 
    let s1 = Seq.toArray s1 |> System.String
    let s2 = Seq.toArray s2 |> System.String
    System.String.Equals(s1, s2, System.StringComparison.CurrentCultureIgnoreCase)
let compareChars isCaseSensitive s1 s2 = 
    let s1 = Seq.toArray s1 |> System.String
    let s2 = Seq.toArray s2 |> System.String
    if isCaseSensitive then
        System.String.Equals(s1, s2)
    else
        System.String.Equals(s1, s2, System.StringComparison.CurrentCultureIgnoreCase)

let rec symbolLiteral isCaseSensitive (s : char seq) cs =
    let length = Seq.length s
    if List.length cs < length then
        []
    else
        let heads, tail = List.splitAt length cs
        if compareChars isCaseSensitive s heads then [(s, tail)]
        else []

let pLiteral s = symbolLiteral true s |> pToken
let pLiteralCI s = symbolLiteral false s |> pToken

let pVarString =
    alphaLower >>= fun first ->
    many0 (alpha <|> digit) >>= fun rest ->
    %(first::rest) |> mreturn
    |> pToken

let pVar =
    pVarString >>= fun str -> Var str |> mreturn

let pFirstUpperAlphaThenManyAlphanumeric = 
    alphaUpper >>= fun first ->
    many0 (alpha <|> digit) >>= fun rest ->
    first::rest |> mreturn
    |> pToken

let pConst = 
    pFirstUpperAlphaThenManyAlphanumeric >>= 
    fun x -> %x |> Const |> mreturn

let pTerm, pBracketsAndTermsList = 
    let pRef = ref pConst
    let pFunc = (fun cs -> !pRef cs)
    let pCommaSeparated = pOneOrMoreWithSep pFunc (pLiteral ",")
    let pBracketsAndTermsList =
        pLiteral "(" >>= fun _ -> 
        (pCommaSeparated <|> mreturn []) >>= fun terms ->
        pLiteral ")" >>.
        mreturn terms
    let rec pFn = 
        pFirstUpperAlphaThenManyAlphanumeric >>= fun name -> 
        pBracketsAndTermsList >>= fun terms ->
        (Fn (%name, terms) |> mreturn)
    
    pRef := pFn <|> pConst <|> pVar
    !pRef, pBracketsAndTermsList

let pPred =
    pFirstUpperAlphaThenManyAlphanumeric >>= fun name ->
    pBracketsAndTermsList <|> mreturn [] >>= fun terms ->
    Pred (%name, terms) |> mreturn

let pTermEqual =
    pThen3 (fun t1 _ t2 -> TermEqual (t1, t2)) pTerm (pLiteral "=") pTerm

let pChain2 pPrim pOp (f : Formula * Formula -> Formula) =
    pPrim >>= fun (x : Formula) ->
    many0 (pOp >>. pPrim) >>= fun (xs : Formula list) ->
    List.fold (fun  (acc : Formula) (elt : Formula)-> 
        f (acc, elt)) x xs
    |> mreturn

let pQuantifier pFormula =
    (pLiteralCI "forall" <|> pLiteralCI "exists") >>= fun q ->
    pOneOrMoreWithSep pVarString (pLiteral ",") >>= fun vars ->
    pFormula >>= fun fm ->
    if compareChars false q "forall" then
        Forall (vars, fm)
    else 
        Exists (vars, fm)
    |> mreturn

let rec pPrim pFormula =
    (pLiteral "~" <|> pLiteralCI "not" >>. (fun r -> pPrim pFormula r) >>= (Not >> mreturn)) <|>
    pTermEqual <|>
    pQuantifier pFormula <|>
    pPred <|>
    (pLiteral "(" >>. pFormula >>= fun expr ->  pLiteral ")" >>. mreturn expr)

let pAlternatives = function
    | p :: ps ->
        List.fold (<|>) p ps
    | _ -> failwith "At least one parser is needed"

let pBinary pFormula pPrim ops construct =
    let pOps = List.map pLiteralCI ops
    pChain2 (pPrim pFormula) (pAlternatives pOps) (fun (x,y) -> Complex (construct, x, y))

let pAnd pFormula =
    pBinary pFormula pPrim ["&"; "^"; "and"] And

let pOr pFormula =
    pBinary pFormula pAnd ["|"; "or"] Or

let pImp pFormula =
    pBinary pFormula pOr ["=>"] Imp

let pIff pFormula =
    pBinary pFormula pImp ["<=>"] Iff
    
let rec pFormula =
    let parserRef = ref (fun _ -> [])
    let parserFunc = (fun cs -> !parserRef cs)
    parserRef := pIff parserFunc
    !parserRef

let formula s = 
    match parsestr pFormula s with
    | [(f, _)] -> f
    | _ -> failwithf "parsing error %s" s

let emptyTheta : Map<string, Term> option = Map.empty |> Some

let rec unify fm1 fm2 theta =
    match fm1, fm2 with
    | Pred (p1, terms1), Pred (p2, terms2) when p1 = p2 ->
        unifyTermsList terms1 terms2 theta
    | TermEqual (term11, term12), TermEqual (term21, term22) ->
        unifyTerms term11 term21 theta
        |> unifyTerms term12 term22
    | Not (subfm1), Not (subfm2) ->
        unify subfm1 subfm2 theta
    | Complex (op1, fm11, fm12), Complex (op2, fm21, fm22) when op1 = op2 ->
        unify fm11 fm21 theta
        |> unify fm12 fm22
    | Forall (vars1, subfm1), Forall (vars2, subfm2) when vars1 = vars2 ->
        unify subfm1 subfm2 theta
    | Exists (vars1, subfm1), Exists (vars2, subfm2) when vars1 = vars2 ->
        unify subfm1 subfm2 theta
    | _ -> None

and unifyTermsList terms1 terms2 theta =
    match terms1, terms2 with
    | [], [] -> theta
    | _ when terms1.Length <> terms2.Length -> None
    | (x::xs), (y::ys) -> 
        unifyTermsList xs ys (unifyTerms x y theta)
    | _ -> failwith "never mind"

and unifyTerms term1 term2 theta =
    let rec binder theta =
        match term1, term2 with
        | Var x, Var y when x = y -> theta |> Some
        | Var x, _ -> 
            match Map.tryFind x theta with
            | Some term when term <> term2 ->
                None
            | Some _ -> theta |> Some
            | None -> 
                Map.add x term2 theta |> Some
        | _, Var _ -> unifyTerms term2 term1 (theta |> Some)
        | Fn (name1, terms1), Fn (name2, terms2) when name1 = name2 ->
            unifyTermsList terms1 terms2 (Some theta)
        | _ when term1 = term2 -> theta |> Some
        | _ -> None

    Option.bind binder theta

let rec unifyLiteral literal1 literal2 theta =
    printfn "unifyLiteral: %A %A %A" literal1 literal2 theta
    match literal1, literal2 with
    | LPred (neg1, name1, terms1), LPred (neg2, name2, terms2)
      when name1 = name2 && neg1 = neg2 ->
        unifyTermsList terms1 terms2 theta
    | LTermEqual (neg1, term11, term12), LTermEqual (neg2, term21, term22)
      when neg1 = neg2 ->
        unifyTermsList [term11; term12] [term21; term22] theta
    | _ -> None
    |> (fun x -> printfn "unfiyLiteral end: %A" x; x)
//////////////////////////////////////////////////////////////////
/// CNF
//////////////////////////////////////////////////////////////////
let rec transformChildren f fm =
    match fm with
    | Complex (op, fm1, fm2) -> Complex (op, f fm1, f fm2)
    | Forall (vars, fm1) -> Forall (vars, f fm1)
    | Exists (vars, fm1) -> Exists (vars, f fm1)
    | Not fm -> f fm |> Not
    | _ -> fm

let rec eliminateImplications fm =
    match fm with
    | Complex (op, fm1, fm2) ->
        let fm1 = eliminateImplications fm1
        let fm2 = eliminateImplications fm2
        match op with
        | Imp -> Complex (Or, Not fm1, fm2)
        | Iff -> 
            Complex (And, Complex (Or, Not fm1, fm2), Complex (Or, Not fm2, fm1))
        | _ -> Complex (op, fm1, fm2)
    | Exists (vars, fm1) ->
        Exists (vars, eliminateImplications fm1)
    | Forall (vars, fm1) ->
        Forall (vars, eliminateImplications fm1)
    | Not fm1 -> eliminateImplications fm1 |> Not
    | _ -> fm

let rec moveNotInwards fm =
    let flipNot = function
        | (Not a) -> a
        | a -> Not a
    let flipAndRecur = flipNot >> moveNotInwards
    match fm with
    | Not (Not a) -> moveNotInwards a
    | Not (Complex (op, fm1, fm2)) when op = And || op = Or ->
        let fm1 = flipAndRecur fm1
        let fm2 = flipAndRecur fm2
        let op = if op = And then Or else And
        Complex (op, fm1, fm2)
    | Not (Forall (vars, fm1)) ->
        Exists (vars, flipAndRecur fm1)
    | Not (Exists (vars, fm1)) ->
        Forall (vars, flipAndRecur fm1)
    | _ -> transformChildren moveNotInwards fm

let rec newVarName var used =
    if Set.contains var used then
        newVarName (var + "'") used
    else var

let rec substituteInTerm substs = function
    | Fn (name, terms) -> Fn (name, List.map (substituteInTerm substs) terms)
    | Var name ->
        match Map.tryFind name substs with
        | Some  other -> Var other
        | _ -> Var name
    | term -> term

let standardizeVars fm =

    let getNewVarsAndSubsts var (vars, used, substs)  =
        if Set.contains var used then
            let newVar = newVarName var used
            let vars1 =  newVar :: vars
            let used1 = Set.add newVar used
            let substs1 = Map.add var newVar substs
            (vars1, used1,  substs1)
        else
            let vars1 = var::vars
            let used1 = Set.add var used
            (vars1, used1, substs)
            
    let rec replaceVars used substs fm  =
        match fm with
        | Forall (vars, fm1) ->
            let (vars, used, newSubsts) =
                List.foldBack getNewVarsAndSubsts vars ([], used, substs)
            let used, fm1 = replaceVars used newSubsts fm1 
            used, Forall (vars, fm1)
        | Exists (vars, fm1) ->
            let (vars, used, newSubsts) =
                List.foldBack getNewVarsAndSubsts vars ([], used, substs)
            let used, fm1 = replaceVars used newSubsts fm1 
            used, Exists (vars, fm1)
        | Pred (name, terms) -> 
            used, Pred (name, List.map (substituteInTerm substs) terms)
        | TermEqual (term1, term2) ->
            used, TermEqual (substituteInTerm substs term1, 
                             substituteInTerm substs term2)
        | Complex (op, fm1, fm2) ->
            let used1, fm1 = replaceVars used substs fm1
            let used2, fm2 = replaceVars used1 substs fm2
            used2, Complex (op, fm1, fm2)
        | Not fm1 ->
            let used1, fm2 = replaceVars used substs fm1
            used1, Not fm2

    replaceVars Set.empty Map.empty fm
    |> snd

let skolemize fm =
    let mutable skolems : Map<string, Term> = Map.empty

    let rec split (str : string) digits =
        if str.Length > 0 then
            if str.[str.Length - 1] |> System.Char.IsDigit then
                split str.[0..str.Length - 2] (str.[str.Length-1..] + digits)
            else
                (str, digits)
        else (str, digits)

    let rec newSkolemName name =
        if Map.containsKey name skolems then 
            let (name, digits) = split name ""
            let n = 
                if digits.Length > 0 
                then LanguagePrimitives.ParseInt32(digits)
                else 0
            newSkolemName (name + (sprintf "%d" (n + 1)))
        else name

    let replaceVarsInTerms = function
        | Var v when Map.containsKey v skolems ->
            Map.find v skolems
        | term -> term

    let rec replaceVarsInFormulas = function
        | Pred (name, terms) ->
            Pred (name, List.map replaceVarsInTerms terms)
        | TermEqual (term1, term2) ->
            TermEqual (replaceVarsInTerms term1,
                       replaceVarsInTerms term2)
        | Complex (op, fm1, fm2) ->
            Complex (op, replaceVarsInFormulas fm1, 
                         replaceVarsInFormulas fm2)
        | Forall (vars, fm1) ->
            Forall (vars, replaceVarsInFormulas fm1)
        | Exists (vars, fm1) ->
            Exists (vars, replaceVarsInFormulas fm1)
        | Not fm1 -> replaceVarsInFormulas fm1 |> Not
    
    let rec go universalVars fm = 
        match fm with
        | Forall (vars, fm1) ->
            let universalVars1 = List.append universalVars vars
            Forall (vars, go universalVars1  fm1)
        | Exists (vars, fm1) -> 
            for var in vars do
                let skolem = newSkolemName (String.map System.Char.ToUpper var)
                skolems <- Map.add var (Fn (skolem, List.map Var universalVars)) skolems
            replaceVarsInFormulas fm1 
            |> go universalVars
        | Not fm1 -> go universalVars fm1 |> Not
        | Complex (op, fm1, fm2) ->
            Complex (op, go universalVars fm1,
                         go universalVars fm2)
        | fm -> fm

    go List.empty fm

let rec removeUniversalQuantifier fm =
    match fm with
    | Forall (_, fm1) -> removeUniversalQuantifier fm1
    | _ -> transformChildren removeUniversalQuantifier fm

let rec distributeAndOverOr fm =
    let f s x = printfn "%s: %s" s (toString x); x

    let rec listToFormula op = function
        | [a] -> a
        | x::xs -> Complex (op, x, listToFormula op xs)
        | _ -> failwith "combinations: cannot be empty list"
    let rec formulaToList = function
    | Complex (And, fm1, fm2) -> List.append (formulaToList fm1) (formulaToList fm2)
    | fm -> [fm]
    let combinations orClauses =
        let folder acc el =
            [ for a in acc do
                match el with
                | Complex (And, fm1, fm2) -> 
                    yield (fm1::a)
                    yield (fm2::a)
                | _ -> yield (el::a) ]
        List.fold folder [[]] orClauses
        |> List.map (List.rev >> listToFormula Or)

    let combs clauses1 clauses2 =
        [for c1 in clauses1 do
            for c2 in clauses2 do
                yield Complex (Or, c1, c2)]

    let rec distributeFmToList = function
        | Complex (And, fm1, fm2) ->
            let fms1 = distributeFmToList fm1
            let fms2 = distributeFmToList fm2
            List.append fms1 fms2
        | Complex (Or, fm1, fm2) ->
            let fms1 = distributeFmToList fm1
            let fms2 = distributeFmToList fm2
            combs fms1 fms2
        | fm -> [fm]


    f "distributeAndOverOr" fm
    fm
    |> distributeFmToList
    |> listToFormula And
    |> f "distributeAndOverOr end"

let formulaToCnf fm =
    let f x = printfn "%s" (toString x); x
    fm
    |> eliminateImplications 
    |> f
    |> moveNotInwards 
    |> f
    |> standardizeVars
    |> f
    |> skolemize 
    |> f
    |> removeUniversalQuantifier 
    |> f
    |> distributeAndOverOr
    |> f

let stringToCnf s =
    s |> formula |> formulaToCnf

let toLiteral = function
    | Pred (name, terms) -> 
        LPred (true, name, terms) 
    | TermEqual (term1, term2) -> 
        LTermEqual (true, term1, term2) 
    | Not (Pred (name, terms)) ->
        LPred (false, name, terms) 
    | Not (TermEqual (term1, term2)) -> 
        LTermEqual (false, term1, term2) 
    | fm -> failwithf "literal is expected but got %A" fm
    
let rec toClause = function
    | Complex (Or, fm1, fm2) -> 
        let clause1 = toClause fm1
        let clause2 = toClause fm2
        Set.union clause1 clause2
    | fm -> 
        fm |> toLiteral |> Set.singleton

let rec cnfToClauses = function
    | Complex (And, fm1, fm2) ->
        let clauses1 = cnfToClauses fm1
        let clauses2 = cnfToClauses fm2
        Set.union clauses1 clauses2
    | fm -> toClause fm |> Set.singleton

let isLiteralPositive = function
    | LPred (v, _, _) -> v
    | LTermEqual (v, _, _) -> v

let rec splitClause clause =
    let folder (positive, negative) literal =
        if isLiteralPositive literal then
            (literal::positive, negative)
        else
            (positive, literal::negative)
         
    match Set.fold folder ([], []) clause with
    | [p], ns -> p, ns
    | _ -> failwith "wrong clause, definitive is expected"

type Kb = 
    { mutable Clauses : Clauses
      mutable Counter : int }

let makeKb () = {Clauses = Set.empty; Counter = 0}

(*
let kbToFormula (kb : Kb) =
    match kb.fms.Count with
    | 0 -> failwith "Empty KB"
    | 1 -> kb.fms.[0]
    | _ -> 
        let mutable fm = kb.fms.[0]
        for fm2 in kb.fms.GetRange(1, kb.fms.Count - 1) do
            fm <- Complex (And, fm, fm2)
        fm
*)

let tell (kb : Kb) s = 
    let clauses = stringToCnf s |> cnfToClauses
    kb.Clauses <- Set.union clauses kb.Clauses

let findSubstitutions (kb : Kb) clauses =
    // clauses = p1 & p2 & .. & pn
    // for each clause in clauses
    //   find in KB pi' such that substition exists
    //   return a list of substitutions
    1

let toPositive negative =
    Seq.map 
        (function
        | LPred (a, b, c) -> LPred (not a, b, c)
        | LTermEqual (a, b, c) -> LTermEqual (not a, b, c))
        negative

let nextVar s (kb : Kb) =
    kb.Counter <- kb.Counter + 1
    s + (sprintf "%d" kb.Counter)

let substituteInLiteral theta literal =
    
    let rec substituteInTerm substs = function
        | Fn (name, terms) -> Fn (name, List.map (substituteInTerm substs) terms)
        | Var name ->
            match Map.tryFind name substs with
            | Some  other -> other
            | _ -> Var name
        | term -> term
        
    match literal with
    | LTermEqual (neg, term1, term2) ->
        LTermEqual (neg, substituteInTerm theta term1, substituteInTerm theta term2)
    | LPred (neg, name, terms) ->
        LPred (neg, name, List.map (substituteInTerm theta) terms)

let standardizeVarsInClause kb clause =
    let rec allVarsInTerm = function
    | Var x -> [x]
    | Fn (name, terms) -> List.collect allVarsInTerm terms
    | _ -> []

    let allVarsInLiteral = function
    | LPred (a, b, terms) -> List.collect allVarsInTerm terms
    | LTermEqual (_, term1, term2) ->
        allVarsInTerm term1
        |>List.append<|
        allVarsInTerm term2

    let allVars = clause |> Set.toList |> List.collect allVarsInLiteral |> List.distinct

    let substs = 
        allVars
        |> List.map (fun v -> (v, nextVar "v" kb |> Var))
        |> Map.ofList

    Set.map (substituteInLiteral substs) clause

let fetchRulesForGoal (kb : Kb) goal theta =
    printfn "fetch: %A %A" goal theta
    [for clause in kb.Clauses do
        let positive, negative = clause |> standardizeVarsInClause kb |> splitClause
        let theta1 = unifyLiteral positive goal theta
        let lhs = toPositive negative
        if theta1.IsSome then
            yield lhs, positive, theta1]
    |> (fun x -> printfn "fetch end: %A" x; x)

let rec folBcOr (kb : Kb) goal theta =
    printfn "OR: %A %A" goal theta
    [for (lhs, rhs, theta) in fetchRulesForGoal kb goal theta do
        yield! folBcAnd kb lhs theta]
        
and folBcAnd kb (goals : Literal seq) thetaOpt =
    printfn "AND: %A %A" goals thetaOpt
    match thetaOpt, Seq.length goals with
    | _, 0 -> [thetaOpt]
    | None, _ -> [None]
    | Some theta, _ ->
        let head = Seq.head goals
        let rest = Seq.tail goals
        [for theta1 in folBcOr kb (substituteInLiteral theta head) thetaOpt do
            for theta2 in folBcAnd kb rest theta1 do
                yield theta2]

let folBcAsk kb s =
    let goal = s |> stringToCnf |> toLiteral
    folBcOr kb goal emptyTheta

//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////

let tests () =
(*
    formula "King(John) AND NOT King(Richard)"
    formula "A & B & C"
    formula "A|C"
    formula "((King(John) AND NOT King(Richard)) OR King(Saladin))"
    formula "FORALL x  King(x)"
    assert (formula "EXISTS x, y  (King(x) AND BrotherOf(x) = y)" = formula "(( (EXISTS x,y  (King(x) AND (BrotherOf(x) = y)) ) ))")
    formula "LegsOf(John,Saladin,Richard)"
    formula "((Missile(m) AND Owns(Nono,m)) => Sells(West , m ,Nono))"
  *)
    unifyTerms (Var "k") (Fn ("grapes", [Var "l"; Const "something"])) (Some Map.empty)

let unifyier () =
    // unify (formula "Plus(A,B) = Plus(B,A)") (formula "Plus(A,B) = Plus(x,y)") (Some Map.empty)
    // unify (formula "Plus(x, A)") (formula ("Plus(B, y)")) (Some Map.empty)

    let unf s1 s2 =
        unify (formula s1) (formula s2) (Some Map.empty)

    unf "((P(A) AND P(B)) OR (P(C) => (P(A) <=> P(C))))" "((P(A) AND P(B)) OR (P(C) => (P(A) <=> P(x))))"

let anmls = "forall x (forall y (Animal(y) => Loves(x,y))) => (exists y Loves(y,x))" |> formula

let kb = makeKb()
let kb2 = makeKb()
tell kb2 "P(Me)"
fetchRulesForGoal kb2 (LPred (true, "P", [Var "x"])) emptyTheta

tell kb "P(x) => Q(x)"
tell kb "P(Me)"
fetchRulesForGoal kb (LPred (true, "Q", [Var "x"])) emptyTheta |> List.head
folBcAsk kb "Q(x)"

(*
American(x) ∧ Weapon(y) ∧ Sells(x, y, z) ∧ Hostile(z) ⇒ Criminal(x) . (9.3)
Owns(Nono, M1) (9.4)
Missile . (M1) (9.5)
Missile(x) ∧ Owns(Nono, x) ⇒ Sells(West, x, Nono) . (9.6)
Missile(x) ⇒ Weapon(x) (9.7)
Enemy(x, America) ⇒ Hostile(x) . (9.8)
American(West) . (9.9)
Enemy(Nono, America) . (9.10
*)
let kb3 = makeKb()
tell kb3 "American(x) & Weapon(y) & Sells(x, y, z) & Hostile(z) => Criminal(x)"
tell kb3 "Owns(Nono, M1)"
tell kb3 "Missile(M1)"
tell kb3 "Missile(x) & Owns(Nono, x) => Sells(West, x, Nono)"
tell kb3 "Missile(x) => Weapon(x)"
tell kb3 "Enemy(x, America) => Hostile(x)"
tell kb3 "American(West)"
tell kb3 "Enemy(Nono, America)"

folBcAsk kb3 "Criminal(West)"

(*
let kb4 = makeKb()
tell kb4 "A<=>A2<=>A3<=>A4<=>A5<=>A6<=>A7"
"A<=>B<=>C"|>stringToCnf|>toString
"A<=>A2"|>stringToCnf|>toString
"((A | ~A) & (~A2 | A2) & (~A2 | ~A) | A3) "|>stringToCnf|>toString
"A<=>A2<=>A3<=>A4<=>A5<=>A6<=>A7"|>stringToCnf|>toString
"A&B|C&D|E" |> formula |> distributeAndOverOr |> toString
"A&B|C" |> formula |> distributeAndOverOr |> toString
"A<=>B<=>C"|>stringToCnf|>toString
standardizeVarsInLiteral kb 
    (Set.ofList [
        LPred (true, "", [Var "x"])
        LTermEqual (true, Var "x", Var "y")
        LTermEqual (true, Var "e", Var "y")
        LTermEqual (true, Fn ("", [Var "e"; Var "y"]), Const "z")
        LTermEqual (true, Fn ("", [Const "e"; Const "y"]), Const "z")
    ])
*)

let kb4 = makeKb()
tell kb4 "forall x (forall y Animal(y) => Loves(x,y)) => (exists y Loves(y,x))"
tell kb4 "forall x (exists z Animal(z) & Kills(x,z)) => (forall y ~Loves(y,x))"
tell kb4 "forall x Animal(x) => Loves(Jack,x)"
tell kb4 "Kills(Jack,Tuna) & Kills(Curiosity,Tuna)"
tell kb4 "Cat(Tuna)"
tell kb4 "forall x Cat(x) => Animal(x)"
tell kb4 "~Kills(Curiosity,Tuna)"