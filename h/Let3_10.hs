module Let3_10 where 

import Parser 
import Control.Applicative ((<|>), many)

-- LET: a simple language
-- AST             
data Program = Program Exp
  deriving (Show)
data Exp = NumExp Int
          | IsZeroExp Exp
          | IfExp Exp Exp Exp
          | VarExp String
          | LetExp String Exp Exp
          -- Arithmetic
          | DiffExp Exp Exp
          | MinusExp Exp
          | AddExp Exp Exp
          | MultExp Exp Exp
          | ModExp Exp Exp
          | EqExp Exp Exp
          | GreaterExp Exp Exp
          | LessExp Exp Exp
          -- List operations
          | ConsExp Exp Exp
          | CarExp Exp
          | CdrExp Exp
          | IsEmptyExp Exp
          | EmptyExp
          | ListExp [Exp]
          | CondExp [(Exp, Exp)]
          deriving (Show)

numExp :: Parser Exp
numExp = do n <- number
            return (NumExp n)

binOp = binaryOp expr
unaryOp name combine = do strTok name
                          strTok "("
                          e <- expr
                          strTok ")"
                          return (combine e)

diffExp :: Parser Exp
diffExp = binOp "-" DiffExp

isZeroExp :: Parser Exp
isZeroExp = do space
               strTok "zero?"
               space
               char '('
               e <- expr
               char ')'
               return (IsZeroExp e)

ifExp :: Parser Exp
ifExp = do strTok "if"
           e1 <- expr
           strTok "then"
           e2 <- expr
           strTok "else"
           e3 <- expr
           return (IfExp e1 e2 e3)

condPart :: Parser (Exp, Exp)
condPart = do e1 <- expr
              strTok "==>"
              e2 <- expr
              return (e1, e2)

condExp :: Parser Exp
condExp = do strTok "cond"
             parts <- (many condPart)
             strTok "end"
             return (CondExp parts)

varExp = do v <- identifier
            return (VarExp v)

letExp = do strTok "let"
            v <- identifier
            strTok "="
            e1 <- expr
            strTok "in"
            e2 <- expr
            return (LetExp v e1 e2)

minusExp = unaryOp "minus" MinusExp

emptyExp = do strTok "empty"
              return (EmptyExp)

--addExp = binary

commaSeparExpList :: Parser [Exp]
commaSeparExpList = 
  (do e <- expr
      (do strTok "," 
          es <- commaSeparExpList
          return (e:es)) 
       <|> return [e]) 
  <|> return []
                   

listExp = do strTok "list"
             strTok "("
             es <- commaSeparExpList
             strTok ")"
             return (ListExp es)

expr = numExp <|> 
  letExp <|> 
  ifExp <|> 
  isZeroExp <|> 
  minusExp <|>
  diffExp <|> 
  (binOp "+" AddExp) <|>
  (binOp "*" MultExp) <|>
  (binOp "mod" ModExp) <|>
  (binOp "equal?" EqExp) <|>
  (binOp "less?" LessExp) <|>
  (binOp "greater?" GreaterExp) <|>
  (binOp "cons" ConsExp) <|>
  (unaryOp "car" CarExp) <|>
  (unaryOp "cdr" CdrExp) <|>
  (unaryOp "empty?" IsEmptyExp) <|>
  emptyExp <|>
  listExp <|>
  varExp

program = do e <- expr
             return (Program e)

-- Expession values
data ExpVal = IntVal Int
            | BoolVal Bool
            | ConsVal ExpVal ExpVal
            | EmptyVal
  deriving (Show)

expValToNum :: ExpVal -> Int
expValToNum (IntVal n) = n
expValToNum _ = error "number expected"
unboxInt = expValToNum

expValToBool :: ExpVal -> Bool
expValToBool (BoolVal v) = v
expValToBool _ = error "bool expected"
unboxBool = expValToBool

valueOfAndToNum :: Exp -> Env -> Int
valueOfAndToNum e env = expValToNum (valueOf e env)

extractAndApply e1 e2 env f =   IntVal (f (valueOfAndToNum e1 env)
           (valueOfAndToNum e2 env))

binOpImpl :: (ExpVal -> a) -> 
             (ExpVal -> b) ->
             (c -> ExpVal) ->
             Exp -> Exp -> Env ->                           
             (a -> b -> c) -> 
             ExpVal 
binOpImpl unbox1 unbox2 box exp1 exp2 env f =
  box (f (unbox1 val1) (unbox2 val2))
  where
    val1 = valueOf exp1 env
    val2 = valueOf exp2 env
binOpImplInt = binOpImpl unboxInt unboxInt IntVal
relOpImpl = binOpImpl unboxInt unboxInt BoolVal
  

-- Environment
type Env = [(String,ExpVal)]

emptyEnv :: Env
emptyEnv = []

extendEnv :: String -> ExpVal -> Env -> Env
extendEnv var val env = ((var,val):env)

applyEnv :: Env -> String -> ExpVal
applyEnv env searchVar = case (lookup searchVar env) of
  Nothing -> error ("variable is not found" ++ searchVar)
  Just val -> val


valueOf :: Exp -> Env -> ExpVal

valueOf (NumExp n) env = (IntVal n)

valueOf (VarExp var) env = (applyEnv env var)

valueOf (IsZeroExp e) env = if num == 0 then (BoolVal True) else (BoolVal False)
  where num = (expValToNum (valueOf e env))

valueOf (IfExp e1 e2 e3) env = if val1 then (valueOf e2 env) else (valueOf e3 env)
  where val1 = (expValToBool (valueOf e1 env))

valueOf (LetExp var e1 e2) env = (valueOf e2 (extendEnv var initVal env))
  where initVal = valueOf e1 env

valueOf (MinusExp e) env = (IntVal (-(expValToNum (valueOf e env))))
valueOf (DiffExp e1 e2) env = extractAndApply e1 e2 env (-)
valueOf (AddExp e1 e2) env = extractAndApply e1 e2 env (+)
valueOf (MultExp e1 e2) env = extractAndApply e1 e2 env (*)
valueOf (ModExp e1 e2) env = binOpImplInt e1 e2 env mod 
valueOf (EqExp e1 e2) env = relOpImpl e1 e2 env (==) 
valueOf (GreaterExp e1 e2) env = relOpImpl e1 e2 env (>) 
valueOf (LessExp e1 e2) env = relOpImpl e1 e2 env (<) 

valueOf (ConsExp e1 e2) env = ConsVal val1 val2
  where val1 = valueOf e1 env
        val2 = valueOf e2 env
valueOf (CarExp e) env = case (valueOf e env) of
  (ConsVal val1 val2) -> val1
  _ -> error "cons cell expected"
valueOf (CdrExp e) env = case (valueOf e env) of
  (ConsVal val1 val2) -> val2
  _ -> error "consCellExpected"
valueOf (IsEmptyExp e) env = case (valueOf e env) of
  EmptyVal -> (BoolVal True)
  (ConsVal _ _) -> (BoolVal False)
  _ -> error "cons cell or empty is expected"
valueOf EmptyExp _ = EmptyVal 
valueOf (ListExp exps) env = 
  foldl (\conses exp -> 
           (ConsVal (valueOf exp env) conses)) 
        EmptyVal (reverse exps)

valueOf' :: String -> Env -> ExpVal
valueOf' inp env = case (parse expr inp) of
                     [] -> error "parse error"
                     [(exp, rest)] -> valueOf exp env

sampleEnv = (extendEnv "x" (IntVal 10) (extendEnv "v" (IntVal 5) (extendEnv "i" (IntVal 1) emptyEnv)))

eval s = valueOf' s sampleEnv
