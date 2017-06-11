module Language

type IsRec = bool
type Name = string

type Alter<'a> = int * 'a list * Expr<'a>
and Expr<'a> =
    | EVar of 'a
    | ENum of int
    | EConstr of int * int
    | EAp of Expr<'a> * Expr<'a>
    | ELet of IsRec * ('a * Expr<'a>) list * Expr<'a>
    | ECase of Expr<'a> * (Alter<'a> list)
    | ELam of 'a list * Expr<'a>

type ScDefn<'a> = Name * 'a list * Expr<'a>

type Program<'a> = ScDefn<'a> list

type CoreExpr = Expr<Name>
type CoreAlt = Alter<Name>
type CoreScDefn = ScDefn<Name>
type CoreProgram = Program<Name>

let bindersOf defns = defns |> List.map fst

let rhssOf defns = defns |> List.map snd

let isAtomicExpr = function
    | EVar _ -> true
    | ENum _ -> true
    | _ -> false

let preludeDefs = 
    [ 
        ("I", ["x"], EVar "x")
        ("K", ["x";"y"], EVar "x")
        ("K1",["x";"y"], EVar "y")
        ("S", ["f";"g";"x"], EAp (EAp ((EVar "f"), (EVar "x")), EAp ((EVar "g"), (EVar "x"))))
        ("compose", ["f";"g";"x"], EAp (EVar "f", EAp (EVar "g", EVar "x")))
        ("twice", ["f"], EAp (EAp (EVar "compose", EVar "f"), (EVar "f"))) 
    ]

type Iseq = 
    | INil
    | IStr of string
    | IAppend of Iseq * Iseq
    | IIdent of Iseq
    | INewline

let space n = System.String(' ', n)


let iNil = INil
let iAppend a b = 
    match a, b with
    | INil, _ -> b
    | _, INil -> a
    | _ -> IAppend (a,b)
let iStr = IStr
let iNewline = INewline
let iIdent iseq = IIdent iseq
let iConcat xs = 
    xs |> List.fold iAppend INil
let rec iInterleave sep = function
    | [] -> INil
    | [iseq] -> iseq
    | iseq :: rest -> iseq |>iAppend<| sep |>iAppend<| (iInterleave sep rest)

let rec flatten col = function
    | [] -> ""
    | (INewline, indent) :: iseqs ->
        "\n" + (space indent) + (flatten indent iseqs)
    | (IIdent iseq, indent) :: iseqs ->
        flatten col ((iseq, col) :: iseqs)
    | (INil, indent) :: iseqs ->  flatten col iseqs
    | (IStr s, indent) :: iseqs -> 
        if s.Contains("\n") then
            let splitted = 
                s.Split('\n') 
                |> List.ofArray 
                |> List.map iStr 
                |> iInterleave INewline
            flatten col ((splitted, indent) :: iseqs)
        else
            s + (flatten (col + s.Length) iseqs)
    | (IAppend (iseq1, iseq2), indent) :: iseqs -> 
        flatten col ((iseq1, indent) :: (iseq2, indent) :: iseqs)

let iDisplay seq = flatten 0 [seq, 0]

let pprVars vars = List.map iStr vars |> iConcat

let rec pprExpr e = 
    match e with    
    | EVar v -> iStr v
    | EAp (e1, e2) -> (pprExpr e1) |>iAppend<| (iStr " ") |>iAppend<| (pprExpr e2)
    | ELet (isrec, defns, expr) ->
        let keyword = if isrec then "let" else "letrec"
        iConcat [ iStr keyword; iNewline;
                  iStr " "; iIdent (pprDefns defns); iNewline;
                  iStr "in "; pprExpr expr ]
    | ECase (expr, altr) ->
        iConcat [ iStr "case"; pprExpr expr; iStr "of"; iNewline;
                  iStr " "; iIdent (pprAlts altr) ]
    | ELam (vars, expr) ->
        iConcat [ iStr "\ "; pprVars vars; iStr " . "; pprExpr expr ]
    
and pprAlts alts = 
    List.map (fun a -> iConcat [ pprAlt a; iStr " ;" ]) alts 
    |> iConcat
and pprAlt (num, vars, expr) =
    iConcat [ iStr "<"; iStr (string num); iStr ">"; iStr " "; pprVars vars; iStr "->"; iNewline;
              iStr " "; iIdent (pprExpr expr) ]
and pprDefns defns = 
    let sep = iConcat [ iStr ";"; iNewline ]
    defns
    |> List.map pprDefn 
    |> iInterleave sep
and pprDefn (name, expr) =
    iConcat [ iStr name; iStr " = "; iIdent (pprExpr expr) ]

    


type Token = char list

let twoCharOps = ["=="; "˜="; ">="; "<="; "->"] |> List.map List.ofSeq

let rec clex = function
    | [] -> []
    | '|' :: '|' :: cs ->
        cs |> List.skipWhile ((<>) '\n') |> clex
    | c1 :: c2 :: cs when List.contains [c1;c2] twoCharOps ->
        [c1;c2] :: clex cs
    | c :: cs when System.Char.IsWhiteSpace c -> clex cs
    | c :: cs when System.Char.IsDigit c ->
        let numTok = c :: List.takeWhile System.Char.IsDigit cs
        let rest = List.skipWhile System.Char.IsDigit cs
        numTok :: (clex rest)
    | c :: cs when System.Char.IsLetter c ->
        let pred = (fun c -> System.Char.IsLetterOrDigit c || c = '_')
        let idTok = 
            c :: List.takeWhile pred cs
        let rest = List.skipWhile pred cs
        idTok :: (clex rest)
    | c :: cs -> [c] :: clex cs

let rec clex2 line = function
    | [] -> []
    | '|' :: '|' :: cs ->
        cs |> List.skipWhile ((<>) '\n') |> clex2 line
    | c1 :: c2 :: cs when List.contains [c1;c2] twoCharOps ->
        (line, [c1;c2]) :: clex2 line cs
    | c :: cs when System.Char.IsWhiteSpace c -> 
        let line = if c = '\n' then line + 1 else line
        clex2 line cs
    | c :: cs when System.Char.IsDigit c ->
        let numTok = c :: List.takeWhile System.Char.IsDigit cs
        let rest = List.skipWhile System.Char.IsDigit cs
        (line, numTok) :: clex2 line rest
    | c :: cs when System.Char.IsLetter c ->
        let pred = (fun c -> System.Char.IsLetterOrDigit c || c = '_')
        let idTok = 
            c :: List.takeWhile pred cs
        let rest = List.skipWhile pred cs
        (line, idTok) :: clex2 line rest
    | c :: cs -> (line, [c]) :: clex2 line cs

type Parser<'a> = Token list -> 'a * (Token List) list

let pLit s = function
    | [] -> []
    | tok :: toks when s = tok -> [s, toks]
    | tok :: toks -> []

let pLitHelp s = pLit (List.ofSeq s)

let keywords = 
    [ "let" 
      "letrec"
      "case"
      "in"
      "of"
      "Pack"
    ] |> List.map List.ofSeq

let pVar = function
    | [] -> []
    | (c :: cs as tok) :: toks 
        when System.Char.IsLetter c && List.contains tok keywords |> not ->
        [tok, toks]
    | _ -> []

let pAlt p1 p2 toks = (p1 toks) |>List.append<| (p2 toks)

let pThen combiner p1 p2 toks =
    [ for (v1, toks1) in p1 toks do
        for (v2, toks2) in p2 toks1 do
            yield combiner v1 v2, toks2 ]

let pThen3 combiner p1 p2 p3 =
    pThen (fun a b -> a b) (pThen combiner p1 p2) p3

let pThen4 combiner p1 p2 p3 p4 =
    pThen (fun a b -> a b) (pThen3 combiner p1 p2 p3) p4
let pThen5 combiner p1 p2 p3 p4 p5 =
    pThen (fun a b -> a b) (pThen4 combiner p1 p2 p3 p4) p5

let pEmpty v toks = [v, toks]

let rec pOneOrMore p toks = 
    [ for (v1, toks1) in p toks do
        match pOneOrMore p toks1 with
        | [] -> yield [v1], toks1
        | xs ->
            for (vs, toks2) in xs do
                yield v1 :: vs, toks2 ]

let pZeroOrMore p toks =
    match pOneOrMore p toks with
    | [] -> [ [], toks ]
    | xs -> xs

let pApply p func toks =
    match p toks with
    | [] -> []
    | xs -> 
        xs
        |> List.map (fun (res, toks1) -> func res, toks1)

let rec pOneOrMoreWithSep p pSep =
    let pSepAndP = pThen (fun _ x -> x) pSep p
    pThen (fun x xs -> x :: xs) p (pZeroOrMore pSepAndP)

let psat func = function
    | [] -> []
    | tok :: toks when func tok ->
        [tok, toks]
    | _ -> []

let pNum =
    psat (function | c :: cs when System.Char.IsNumber c -> true | _ -> false )
    |> pApply <| (fun x -> System.String.Concat(Array.ofList x) |> int32)

let pBetween pLeft pCore pRight  =
    pThen3 (fun _ x _ -> x) pLeft pCore pRight
//type ScDefn<'a> = Name * 'a list * Expr<'a>

let pPack<'a> =
    pThen (fun _ x -> EConstr x) 
        (pLit ("Pack" |> List.ofSeq)) 
        (pBetween (pLit ['{']) 
            (pThen3 (fun x _ y -> x, y) pNum (pLit [',']) pNum) 
            (pLit ['}']))


let pLet pExpr = 
    let pDefn = pThen3 (fun name _ body -> name, body) pVar (pLitHelp "=") pExpr
    let pDefns = pOneOrMoreWithSep pDefn (pLitHelp ",")

    (pThen4 (fun _ defns _ body -> ELet (false, defns, body))
        (pLitHelp "let")
        pDefns
        (pLitHelp "in")
        pExpr) |>pAlt<|
    (pThen4 (fun _ defns _ body -> ELet (true, defns, body))
        (pLitHelp "letrec")
        pDefns
        (pLitHelp "in")
        pExpr)

let pCase pExpr =
    let pAltern = 
        pThen4 (fun num vars _ expr -> 
                printfn "Alter: %d" num
                num, vars, expr)
            (pBetween (pLitHelp "<") pNum (pLitHelp ">"))
            (pZeroOrMore pVar)
            (pLitHelp "->")
            pExpr
    let pAlterns = pOneOrMoreWithSep pAltern (pLitHelp ";")
    pThen4 (fun _ expr _ alts -> ECase (expr, alts)) 
        (pLitHelp "case")
        pExpr
        (pLitHelp "of")
        pAlterns

let pLambda pExpr =
    pThen4 (fun _ vars _ body -> ELam (vars, body))
        (pLitHelp "\\")
        (pOneOrMore pVar)
        (pLitHelp ".")
        pExpr
let  pAexpr pExpr =
    (pApply pVar EVar) |>pAlt<| 
    (pApply pNum ENum) |>pAlt<|
    pPack |>pAlt<|
    pBetween (pLit ['(']) pExpr (pLit [')']) 

type PartialExpr =
    | NoOp
    | FoundOp of Name * CoreExpr

let pApplic pExpr = 
    pOneOrMore (pAexpr pExpr) |>pApply<|
    (fun xs -> List.fold (fun acc elt -> EAp (acc, elt)) (List.head xs) (List.tail xs))
    
let rec pChainl pElt pOp =
    let accum x ops =
        List.fold (fun acc (op, y) -> op acc y) x ops
    pThen accum
        pElt
        (pZeroOrMore (pThen (fun op elt -> (op, elt)) 
            pOp
            pElt))

let pMult pExpr =
    pChainl (pApplic pExpr) 
        (pApply (pLitHelp "*") (fun _ -> fun x y -> EAp (EAp (EVar ['*'], x), y)))

let rec pExpr<'a> = 
    let p = (fun toks -> pExpr toks)
    pLet p |>pAlt<|
    pCase p |>pAlt<|
    pLambda p |>pAlt<|
    pMult p

let pSc = 
    let mkSc name args _ expr = (name, args, expr)
    pThen4 mkSc pVar (pZeroOrMore pVar) (pLit ['=']) pExpr

let pProgram =
    pOneOrMoreWithSep pSc (pLitHelp ";")

let parse s = s |> List.ofSeq |> clex |> pProgram
let parseExpr s = s |> List.ofSeq |> clex |> pExpr