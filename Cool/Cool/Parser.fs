module Parser

open CoolAst
open FParsec

type P<'a> = Parser<'a, unit>

let commentLine<'r> = skipString "--" >>. skipManyTill anyChar newline
let commentBlock<'r> = skipString "(*" >>. skipManyTill anyChar (skipString "*)")

let ws = 
    choice
        [ spaces1
          commentLine
          commentBlock ]
    |> many
    |>> ignore
    
let wsSurround p =  p .>> ws
let stringWs s = pstring s .>> ws

// let wsSurround p = between spaces spaces p

let pCoolString : P<ExprInner> = 
    skipChar '"' >>. manyTill (pstring "\\\"" <|> (anyChar |>> string)) (skipChar '"')
    |> wsSurround
    |>> (List.toArray >> System.String.Concat >> String)

let pCoolInt : P<ExprInner> = 
    let main = parse {
        let! n = puint32
        if n > (uint32 System.Int32.MaxValue)
        then return! fail "Too big integer constant"
        else return (int n)
    }
    main |>> Integer |> wsSurround

let getLine : Parser<int, unit> = parse {
    let! pos = getPosition
    return int pos.Line
}

let keywords = 
    [ 
        "class"
        "else"
        "false"
        "fi"
        "if"
        "in"
        "inherits"
        "isvoid"
        "let"
        "loop"
        "pool"
        "then"
        "while"
        "case"
        "esac"
        "new"
        "of"
        "not"
        "true" ]

let keyword s = 
    pstring s
    .>> notFollowedBy letter
    .>> notFollowedBy digit
    .>> notFollowedBy (pchar '_')
    |> wsSurround
    |> attempt

let pId : P<Id> = parse {
        let start = fun c -> isAsciiLetter c || c = '_'
        let cont = (fun c -> isAsciiLetter c || isDigit c || c = '_')
        let! v = identifier (IdentifierOptions(isAsciiIdStart = start, isAsciiIdContinue = cont ))
        let! line = getLine
        if List.contains v keywords |> not then
            return (line, v)
    }

let pName: P<Id> = parse {
    let! n = pId
    if (snd n).[0] |> System.Char.IsUpper
    then return! fail "lower case identifier is expected"
    else return n
}
let pType: P<Id> = parse {
    let! n = pId
    if (snd n).[0] |> System.Char.IsUpper
    then return n
    else return! fail "upper case identifier is expected"
}

let pNew: P<ExprInner> =
    stringWs "new" >>.
    (wsSurround pId) |>> New

let expr, exprRef = createParserForwardedToRef()

let parenExpr = between (stringWs "(") (stringWs ")") expr

let idArglist =
    pipe2
        (pName .>> ws)
        (between 
            (pchar '(' |> wsSurround) 
            (pchar ')' |> wsSurround)
            (sepBy expr (pchar ',' |> wsSurround)))
        (fun name args ->
            name, args)

let makeExpr loc exprInner = 
    { Type = None; Loc = loc; Expr = exprInner }

let dotIdArgslist =
    (pchar '.' |> wsSurround) 
    >>. idArglist
    |>> (fun (n, a) ->
            (fun expr -> DynDispatch (expr, n, a) |> makeExpr expr.Loc))

let staticDispatchPart =
    pipe2
        ((pchar '@' |> wsSurround) >>. (wsSurround pType))
        ((pchar '.' |> wsSurround) >>. idArglist)
        (fun typ (n, a) ->
            (fun expr -> StatDispatch (expr, typ, n, a) |> makeExpr expr.Loc))

let selfDispatch =
    idArglist |>> SelfDispatch |> attempt

let ifExpr =
    pipe3
        (stringWs "if" >>. expr)
        (stringWs "then" >>. expr)
        (stringWs "else" >>. expr .>> stringWs "fi")
        (fun e1 e2 e3 -> If (e1, e2, e3))

let whileExpr =
    pipe2
        (keyword "while" >>. expr)
        (keyword "loop" >>. expr .>> keyword "pool")
        (fun e1 e2 -> While (e1, e2))

let letExpr =
    let binding =
        pipe3
            (pName |> wsSurround .>> stringWs ":")
            (pType |> wsSurround)
            (stringWs "<-" >>. expr |> opt)
            (fun v t eOpt -> (v, t, eOpt))
    pipe2
        (keyword "let" >>. sepBy1 binding (stringWs ","))
        (keyword "in" >>. expr)
        (fun bs e -> Let (bs, e))

let caseExpr =
    let case =
        pipe3 
            (pName |> wsSurround .>> stringWs ":")
            (pType |> wsSurround .>> stringWs "=>")
            (expr .>> stringWs ";")
            (fun v t e -> v,t,e)
    pipe2 
        (keyword "case" >>. expr .>> keyword "of")
        (case |> attempt |> many1 .>> keyword "esac")
        (fun e cs -> Case (e, cs))

let assign =
    pipe2
        (wsSurround pId)
        ((stringWs  "<-" >>. expr) |> opt)
        (fun v eOpt ->
            match eOpt with
            | None -> Identifier v
            | Some e -> Assign (v, e))

let blockExpr =
    expr .>> stringWs ";" |> many1
    |> between (stringWs "{") (stringWs "}")
    |>> Block

let trueFalse =
    choice
        [ stringWs "true" >>. preturn True
          stringWs "false" >>. preturn False ]

let exprInnerToExpr p =
    pipe2 getLine p (fun line e -> 
        { Type = None; Expr = e; Loc = line })

let primary<'r> =
    let choices = 
        [ pCoolInt
          pCoolString          
          ifExpr
          whileExpr
          letExpr
          caseExpr
          blockExpr
          selfDispatch
          pNew
          trueFalse
          assign 
          pName |>> Identifier ]
    choices
    |> List.map exprInnerToExpr
    |> (fun xs -> parenExpr :: xs)
    |> choice

let unary<'r> =
    let un, unRef = createParserForwardedToRef()
    let isvoid = 
        (stringWs "isvoid") 
        >>. un 
        |>> Isvoid |> exprInnerToExpr
    let negate =
        (wsSurround (pchar '~')) 
        >>. un 
        |>> Negate |> exprInnerToExpr

    unRef := choice
        [ isvoid
          negate
          primary ]
    un

let postfix = 
    choice 
        [ dotIdArgslist
          staticDispatchPart ]

let postfixExpr = 
    pipe2 unary (many postfix)
        (fun prim posts ->
            posts
            |> List.fold (fun acc elt ->
                elt acc) prim)

let operations =
    [ [ "*", Times; "/", Divide ]
      [ "+", Plus; "-", Minus ]
      [ "<=", LE; "<", LT; "=", EQ ] ]

let binaryExprs =
    operations
    |> List.fold (fun acc elt ->
        elt 
        |> List.map (fun (s, constr) ->
            (stringWs s >>. (preturn (fun e1 e2 ->
                constr (e1, e2) |> makeExpr e1.Loc))))
        |> choice
        |> chainl1 acc) postfixExpr

let notExpr = 
    (keyword "not") 
    >>. expr 
    |>> Not |> exprInnerToExpr

exprRef := choice
    [ notExpr
      binaryExprs ]

let formal<'r> =
    pipe2
        (pName |> wsSurround .>> stringWs ":")
        (pType |> wsSurround)
        (fun a b -> a,b)

let methodDef<'r> =
    let formalList =
        sepBy formal (stringWs ",")
        |> between (stringWs "(") (stringWs ")")
    let body =
        expr
        |> between (stringWs "{") (stringWs "}")
    pipe4
        (wsSurround pId)
        formalList
        (stringWs ":" >>. (wsSurround pType))
        body
        (fun m formals t body -> Method (m, formals, t, body))
    |> attempt

let attribute =
    pipe3         
        (pName |> wsSurround .>> stringWs ":")
        (pType |> wsSurround)
        (stringWs "<-" >>. expr |> opt)
        (fun v t eOpt -> Attribute (v,t,eOpt))
    |> attempt

let pclass =
    let features =
        (attribute <|> methodDef) .>> stringWs ";" |> many
        |> between (stringWs "{") (stringWs "}") 
    pipe3
        (keyword "class" >>. (wsSurround pType) |> attempt)
        (keyword "inherits" >>. (wsSurround pType) |> opt)
        features
        (fun t tOpt fs -> Class (t, tOpt, fs))

let cool =
    ws >>. many (wsSurround (pclass .>> stringWs ";")) |>> Ast


let parse (s:string) : Ast option =
    match FParsec.CharParsers.run cool s with
    | FParsec.CharParsers.ParserResult.Success (ast, _, _) -> Some ast
    | FParsec.CharParsers.ParserResult.Failure (err, _, _) -> 
        printfn "%s" err
        None