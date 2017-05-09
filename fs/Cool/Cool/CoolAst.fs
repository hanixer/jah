module CoolAst    

type Id = int * string
type Formal = Id * Id

type Ast = Ast of Class list
and Class = Class of Id * Id option * Feature list
and Feature = 
    | Method of Id * Formal list * Id * Expr
    | Attribute of Id * Id * Expr option
and Expr = int * ExprInner
and ExprInner =
    | Assign of Id * Expr
    | DynDispatch of Expr * Id * Expr list
    | StatDispatch of Expr * Id * Id * Expr list
    | SelfDispatch of Id * Expr list
    | If of Expr * Expr * Expr
    | While of Expr * Expr
    | Block of Expr list
    | Let of (Id * Id * Expr option) list * Expr
    | Case of Expr * (Id * Id * Expr) list
    | New of Id
    | Isvoid of Expr
    | Plus of Expr * Expr
    | Minus of Expr * Expr
    | Times of Expr * Expr
    | Divide of Expr * Expr
    | LT of Expr * Expr
    | LE  of Expr * Expr
    | EQ of Expr * Expr
    | Not of Expr
    | Negate of Expr
    | Integer of int
    | String of string
    | Identifier of Id
    | True
    | False

module Deserialize =
    open FParsec

    let pNumEndline<'b> : Parser<int32, 'b> = pint32 >>= (fun n -> newline >>. preturn n)
    let pTrue<'r> : Parser<ExprInner, 'r> = pstring "true" >>. newline >>. preturn True
    let pFalse<'r> : Parser<ExprInner, 'r> = pstring "false" >>. newline >>. preturn False
    let pId<'r> : Parser<Id, 'r> = parse {
        let start = fun c -> isAsciiLetter c || c = '_'
        let cont = (fun c -> isAsciiLetter c || isDigit c || c = '_')
        let! line = pNumEndline
        let! id = identifier (IdentifierOptions(isAsciiIdStart = start, isAsciiIdContinue = cont ))
        do! skipNewline
        return (line, id)
    } 

    let pExpr, pExprRef = createParserForwardedToRef()
    let pList p = parse {
        let! n = pNumEndline
        let! results = parray n p
        return List.ofArray results
    }
    let pExprList<'r> = pList pExpr
    let pString<'r> : Parser<ExprInner, 'r> = 
        pstring "string" >>. newline
        >>. manyTill anyChar newline 
        |>> (fun cs -> new string (cs |> List.toArray) |> ExprInner.String)
    let pInteger<'r> : Parser<ExprInner, 'r> = 
        pstring "integer" >>. newline
        >>. pNumEndline |>> Integer

    let skipStringNewline str = parse {
        do! skipString str
        do! skipNewline
    }
    let pIdentifier<'r> : Parser<ExprInner, 'r> = parse {
        do! skipStringNewline "identifier"
        let! id = pId
        return Identifier(id)
    }
    let pUnaryOp str constr = parse { 
        do! skipStringNewline str
        let! e = pExpr
        return constr(e)
    }

    let pBinaryOp str constr = parse { 
        do! skipStringNewline str
        let! e1 = pExpr
        let! e2 = pExpr
        return constr(e1, e2)
    }

    let pNegate<'r> = pUnaryOp "negate" Negate
    let pNot<'r> = pUnaryOp "not" Not
    let pIsvoid<'r> = pUnaryOp "isvoid" Isvoid
    let pPlus<'r> = pBinaryOp "plus" Plus
    let pMinus<'r> = pBinaryOp "minus" Minus
    let pTimes<'r> = pBinaryOp "times" Times
    let pDivide<'r> = pBinaryOp "divide" Divide
    let pLT<'r> = pBinaryOp "lt" LT
    let pLE<'r> = pBinaryOp "le" LE
    let pEQ<'r> = pBinaryOp "eq" EQ
    let pAssign<'r> = parse {
        do! skipStringNewline "assign"
        let! id = pId
        let! e = pExpr
        return Assign(id, e)
    }
    let pDynDispatch<'b> = parse {
        do! skipStringNewline "dynamic_dispatch"
        let! e = pExpr
        let! meth = pId
        let! args = pExprList
        return DynDispatch(e, meth, args)
    }
    let pStatDispatch<'b> = parse {
        do! skipStringNewline "static_dispatch"
        let! e = pExpr
        let! t = pId
        let! meth = pId
        let! args = pExprList
        return StatDispatch(e, t, meth, args)
    }
    let pSelfDispatch<'b> = parse {
        do! skipStringNewline "self_dispatch"
        let! meth = pId
        let! args = pExprList
        return SelfDispatch(meth, args)
    }
    let pIf<'b> = parse {
        do! skipStringNewline "if"
        let! pred = pExpr
        let! th = pExpr
        let! el = pExpr
        return If(pred, th, el)
    }
    let pWhile<'b> = parse {
        do! skipStringNewline "while"
        let! pred = pExpr
        let! body = pExpr
        return While(pred, body)
    }
    let pBlock<'b> = parse {
        do! skipStringNewline "block"
        let! es = pExprList
        return Block(es)
    }
    let pVarType<'r> = parse {
        let! v = pId
        let! t = pId
        return (v, t)
    }
    let pLet<'r> = 
        let pNoBind = parse {
            do! skipStringNewline "let_binding_no_init"
            let! v, t = pVarType
            return v, t, None
        }
        let pBind = parse {
            do! skipStringNewline "let_binding_init"
            let! v, t = pVarType
            let! e = pExpr
            return v, t, Some e
        }
        parse {
        do! skipStringNewline "let"
        let! bindList = pList (pNoBind <|> pBind)
        let! e = pExpr
        return Let(bindList, e)
    }
    let pCase<'r> = 
        let pCaseElem = parse {
            let! v, t = pVarType
            let! e = pExpr
            return (v, t, e)
        }
        parse {
        do! skipStringNewline "case"
        let! e = pExpr
        let! elems = pList pCaseElem
        return Case(e, elems)
    }
    let pNew<'b> = skipStringNewline "new" >>. pId |>> New
    let pExprInner<'r> = 
        pAssign <|> 
        pDynDispatch <|> 
        pStatDispatch <|>
        pSelfDispatch <|>
        pIf <|>
        pWhile <|>
        pBlock <|>
        pLet <|>
        pCase <|>
        pNew <|>
        pIsvoid <|> 
        pPlus <|>
        pMinus <|>
        pTimes <|> 
        pDivide <|> 
        pLT <|> 
        pLE <|> 
        pEQ <|>
        pNot <|> 
        pNegate <|> 
        pInteger <|> 
        pString <|> 
        pIdentifier <|>
        pTrue <|>
        pFalse

    pExprRef := parse {
        let! line = pNumEndline
        let! inner = pExprInner
        return (line, inner)
    }

    let pAttributeNo<'r> = parse {
        do! skipStringNewline "attribute_no_init"
        let! v, t = pVarType
        return Attribute(v, t, None)
    }
    let pAttribute<'r> = parse {
        do! skipStringNewline "attribute_init"
        let! v, t = pVarType
        let! e = pExpr
        return Attribute(v, t, Some e)
    }
    let pMethod<'r> = 
        parse {
        do! skipStringNewline "method"
        let! id = pId
        let! formals = pList pVarType
        let! t = pId
        let! b = pExpr
        return Method(id, formals, t, b)
    }
    let pFeature<'r> = 
        pAttributeNo <|>
        pAttribute <|> 
        pMethod
    let pClass<'r> = parse {
        let! id = pId
        let! inh = 
            skipStringNewline "no_inherits" >>. preturn None
            <|> (skipStringNewline "inherits" >>. pId |>> Some)
        let! fs = pList pFeature
        return Class(id, inh, fs)    
    }

    let pAst : Parser<Class list, unit> = pList pClass
    let x = (do());true
    let initIt n = 
        printfn "YOU %d ARE NEVER SO GOOOOOOOOOOOOOOOD" n
        do pExprRef := parse {
            let! line = pNumEndline
            let! inner = pExprInner
            return (line, inner)
        }
        1