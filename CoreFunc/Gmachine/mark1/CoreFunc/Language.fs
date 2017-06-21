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

let space n = if n >= 0 then  System.String(' ', n) else ""


let iNil = INil
let iAppend a b = 
    match a, b with
    | INil, _ -> b
    | _, INil -> a
    | _ -> IAppend (a,b)
let iStr = IStr
let iNewline = INewline
let iIndent iseq = IIdent iseq
let iConcat xs = 
    xs |> List.fold iAppend INil
let rec iInterleave sep = function
    | [] -> INil
    | [iseq] -> iseq
    | iseq :: rest -> iseq |>iAppend<| sep |>iAppend<| (iInterleave sep rest)

let showAddr = string >> iStr

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

let iNum n = iStr (string n)

let iFWNum width n =
    let digits = string n
    iStr (space (width - Seq.length digits) + digits)

let iLayn seqs = 
    let layItem n iseq = 
        iConcat [ iFWNum 4 n; iStr ") "; iIndent iseq; iNewline ]
    iConcat (List.mapi layItem seqs)

let pprVars vars = List.map iStr vars |> iConcat

let rec pprExpr e = 
    match e with    
    | EVar v -> iStr v
    | EAp (e1, e2) -> (pprExpr e1) |>iAppend<| (iStr " ") |>iAppend<| (pprExpr e2)
    | ELet (isrec, defns, expr) ->
        let keyword = if isrec then "let" else "letrec"
        iConcat [ iStr keyword; iNewline;
                  iStr " "; iIndent (pprDefns defns); iNewline;
                  iStr "in "; pprExpr expr ]
    | ECase (expr, altr) ->
        iConcat [ iStr "case"; pprExpr expr; iStr "of"; iNewline;
                  iStr " "; iIndent (pprAlts altr) ]
    | ELam (vars, expr) ->
        iConcat [ iStr "\\ "; pprVars vars; iStr " . "; pprExpr expr ]
    | _ -> INil
    
and pprAlts alts = 
    List.map (fun a -> iConcat [ pprAlt a; iStr " ;" ]) alts 
    |> iConcat
and pprAlt (num, vars, expr) =
    iConcat [ iStr "<"; iStr (string num); iStr ">"; iStr " "; pprVars vars; iStr "->"; iNewline;
              iStr " "; iIndent (pprExpr expr) ]
and pprDefns defns = 
    let sep = iConcat [ iStr ";"; iNewline ]
    defns
    |> List.map pprDefn 
    |> iInterleave sep
and pprDefn (name, expr) =
    iConcat [ iStr name; iStr " = "; iIndent (pprExpr expr) ]

    


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
(*
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
*)
// type Parser<'a> = Token list -> 'a * (Token List) list

let psat func = function
    | [] -> []
    | tok :: toks when func tok ->
        [tok, toks]
    | _ -> []

let pLitHelp s = psat ((=) s)

let keywords = 
    [ "let" 
      "letrec"
      "case"
      "in"
      "of"
      "Pack"
    ] //|>  Seq.ofList //|> List.map List.ofSeq

let pVar toks = 
    match toks with
    | [] -> []
    | tok :: toks 
        when Seq.head tok |> System.Char.IsLetter && List.contains tok keywords |> not ->
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

let pNum : string list -> (int * string list) list =
    psat (Seq.head >> System.Char.IsNumber)
    |> pApply <| int

let pBetween pLeft pCore pRight  =
    pThen3 (fun _ x _ -> x) pLeft pCore pRight
//type ScDefn<'a> = Name * 'a list * Expr<'a>

let pPack<'a> =
    pThen (fun _ x -> EConstr x) 
        (pLitHelp "Pack")
        (pBetween (pLitHelp "{") 
            (pThen3 (fun x _ y -> x, y) pNum (pLitHelp ",") pNum) 
            (pLitHelp "}"))


let pLet pExpr = 
    let pDefn = pThen3 (fun name _ body -> name, body) pVar (pLitHelp "=") pExpr
    let pDefns = pOneOrMoreWithSep pDefn (pLitHelp ";")

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
    pPack |>pAlt<|
    (pApply pVar EVar) |>pAlt<| 
    (pApply pNum ENum) |>pAlt<|
    pBetween (pLitHelp "(") pExpr (pLitHelp ")") 

type PartialExpr =
    | NoOp
    | FoundOp of Name * CoreExpr

let pApplication pExpr = 
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

let pBinaryl pExpr primitive ops =
    let ops = List.map pLitHelp ops
    let pCombined =
        if List.length ops = 1 then List.head ops
        else List.fold pAlt ops.Head ops.Tail
    pChainl (primitive pExpr) 
        (pApply pCombined (fun op -> fun x y -> EAp (EAp (EVar op, x), y)))

let pMult pExpr =
    pBinaryl pExpr pApplication ["*"; "/"]

let pAdd pExpr =
    pBinaryl pExpr pMult ["+"; "-"]

//relop → < | <= | == | ˜= | >= | > Comparison

let pRelop =
    let ops = ["<="; "<"; "=="; "~="; ">="; ">"] |> List.map pLitHelp
    List.fold pAlt ops.Head ops.Tail

let pRelopExpr pExpr =
    let p1 =
        pThen3 (fun x op y -> EAp (EAp (EVar op, x), y))
            (pAdd pExpr) pRelop (pAdd pExpr)
    let p2 = pAdd pExpr
    p1 |>pAlt<| p2

let pAnd pExpr =
    pBinaryl pExpr pRelopExpr ["&"]

let pOr pExpr =
    pBinaryl pExpr pAnd ["|"]

let rec pExpr<'a> =
    let parserReference = ref (fun _ -> [ENum 2, []])
    let p = (fun toks -> !parserReference toks)
    parserReference := 
        pLet p |>pAlt<|
        pCase p |>pAlt<|
        pLambda p |>pAlt<|
        pOr p
        // pAdd p
    
    !parserReference

    

let pSc = 
    let mkSc name args _ expr = (name, args, expr)
    pThen4 mkSc pVar (pZeroOrMore pVar) (pLitHelp "=") pExpr

let pProgram =
    pOneOrMoreWithSep pSc (pLitHelp ";")

let parse s = 
    s 
    |> List.ofSeq 
    |> clex |> List.map (List.toArray >> System.String)
    |> pProgram 
    |> List.tryHead |> Option.map fst
    |> defaultArg <| []

// let parseExpr s = s |> List.ofSeq |> clex |> pExpr