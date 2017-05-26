module Parser

open CoolAst
open FParsec

let ws = spaces
let wsSurround p =  p .>>  spaces
// let wsSurround p = between spaces spaces p

let pCoolString : Parser<ExprInner, unit> = 
    (regex "\"[^\n]*\"" |>> String) |> wsSurround 

let pCoolInt : Parser<ExprInner, unit> = 
    let main = parse {
        let! n = puint32
        if n > (uint32 System.Int32.MaxValue)
        then return! fail "Too big integer constant"
        else return (int n)
    }
    main |>> Integer |> wsSurround

let getLine<'r> : Parser<int, 'r> = parse {
    let! pos = getPosition
    return int pos.Line
}

let pId<'r> : Parser<Id, 'r> = parse {
        let start = fun c -> isAsciiLetter c || c = '_'
        let cont = (fun c -> isAsciiLetter c || isDigit c || c = '_')
        let! id = identifier (IdentifierOptions(isAsciiIdStart = start, isAsciiIdContinue = cont ))
        let! line = getLine
        return (line, id)
    }
let pName<'r> : Parser<Id, 'r> = parse {
    let! n = pId
    if (snd n).[0] |> System.Char.IsUpper
    then return! fail "lower case identifier is expected"
    else return n
}
let pType<'r> : Parser<Id, 'r> = parse {
    let! n = pId
    if (snd n).[0] |> System.Char.IsUpper
    then return n
    else return! fail "upper case identifier is expected"
}

let pNew<'r> : Parser<ExprInner, 'r> =
    wsSurround (pstring "new") >>.
    pId |>> New >>= preturn 


let opp = OperatorPrecedenceParser<Expr, unit, unit>()
let expr, exprRef = createParserForwardedToRef()
// let expr = opp.ExpressionParser

let idArglist =
    pipe2
        (pName .>> ws)
        (between 
            (pchar '(' |> wsSurround) 
            (pchar ')' |> wsSurround)
            (sepBy (expr |> wsSurround) (pchar ',' |> wsSurround)))
        (fun name args ->
            name, args)

let makeExpr loc exprInner = 
    { Type = None; Loc = loc; Expr = exprInner }

let dotIdArgslist =
    (pchar '.' |> wsSurround) 
    >>. idArglist
    |>> (fun (n, a) ->
            (fun expr -> DynDispatch (expr, n, a) |> makeExpr expr.Loc))

let ifExpr =
    pipe3
        (wsSurround (pstring "if") >>. expr)
        (wsSurround (pstring "then") >>. expr)
        (wsSurround (pstring "else") >>. expr .>> (wsSurround (pstring "fi")))
        (fun e1 e2 e3 -> If (e1, e2, e3))

let exprInnerToExpr p =
    pipe2 p getLine (fun e line -> 
        { Type = None; Expr = e; Loc = line })

let primary<'r> =
    let choices = 
        [ pCoolInt
          ifExpr ]
    choices
    |> List.map exprInnerToExpr
    |> choice

let postfix = dotIdArgslist
// f().f().f()
// 
let primaryAndPostfix = 
    pipe2 primary (many postfix)
        (fun prim posts ->
            posts
            |> List.fold (fun acc elt ->
                elt acc) prim)


let mulChain =
    chainl1 primaryAndPostfix 
        (pchar '*' |> wsSurround >>. (preturn (fun e1 e2 ->
        Times (e1, e2) |> makeExpr e1.Loc)))

let addChain =
    chainl1 mulChain
        (pchar '+' |> wsSurround >>. (preturn (fun e1 e2 ->
        Plus (e1, e2) |> makeExpr e1.Loc)))

exprRef := addChain
(*
let primary<'r> = 
    choice [
        // add if, while, etc. here
        pCoolInt
        pCoolString
        pNew
        wsSurround (pstring "true") >>. preturn True
        wsSurround (pstring "false") >>. preturn False
        between (wsSurround (pstring "(")) (wsSurround (pstring ")")) expr
        pId |>> Identifier >>= preturn
    ]
*)
(*
    E :=    E + E
            E * E
            if E then E else E fi
            int
            string
            E.ID(...)

    primary
    postfix
    simpleExpr = prim post pipe2
*)



