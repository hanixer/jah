module Let3_6 where 

import Parser 
import Control.Applicative ((<|>))

-- LET: a simple language
-- AST             
data Program = Program Exp
  deriving (Show)
data Exp = NumExp Int
          | DiffExp Exp Exp
          | IsZeroExp Exp
          | IfExp Exp Exp Exp
          | VarExp String
          | LetExp String Exp Exp
          | MinusExp Exp
          deriving (Show)

numExp :: Parser Exp
numExp = do n <- number
            return (NumExp n)

diffExp :: Parser Exp
diffExp = do strTok "-("
             e1 <- expr
             char ','
             e2 <- expr
             char ')'
             return (DiffExp e1 e2)

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

varExp = do v <- identifier
            return (VarExp v)

letExp = do strTok "let"
            v <- identifier
            strTok "="
            e1 <- expr
            strTok "in"
            e2 <- expr
            return (LetExp v e1 e2)

minusExp = do strTok "minus("
              e <- expr
              strTok ")"
              return (MinusExp e)

expr = numExp <|> 
  letExp <|> 
  ifExp <|> 
  diffExp <|> 
  isZeroExp <|> 
  minusExp <|>
  varExp

program = do e <- expr
             return (Program e)

-- Expession values
data ExpVal = IntVal Int
            | BoolVal Bool
  deriving (Show)

expValToNum :: ExpVal -> Int
expValToNum (IntVal n) = n
expValToNum _ = error "number exp expected"

expValToBool :: ExpVal -> Bool
expValToBool (BoolVal v) = v
expValToBool _ = error "bool expected"

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

valueOf (DiffExp e1 e2) env = (IntVal ((expValToNum (valueOf e1 env)) - 
                                       (expValToNum (valueOf e2 env))))

valueOf (IsZeroExp e) env = if num == 0 then (BoolVal True) else (BoolVal False)
  where num = (expValToNum (valueOf e env))

valueOf (IfExp e1 e2 e3) env = if val1 then (valueOf e2 env) else (valueOf e3 env)
  where val1 = (expValToBool (valueOf e1 env))

valueOf (LetExp var e1 e2) env = (valueOf e2 (extendEnv var initVal env))
  where initVal = valueOf e1 env

valueOf (MinusExp e) env = (IntVal (-(expValToNum (valueOf e env))))

valueOf' :: String -> Env -> ExpVal
valueOf' inp env = case (parse expr inp) of
                     [] -> error "parse error"
                     [(exp, rest)] -> valueOf exp env

sampleEnv = (extendEnv "x" (IntVal 10) (extendEnv "v" (IntVal 5) (extendEnv "i" (IntVal 1) emptyEnv)))
