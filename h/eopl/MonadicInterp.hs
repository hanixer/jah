module Let where 

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
          | PrintExp Exp
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

printExp = do
  strTok "print"
  strTok "("
  e <- expr
  strTok ")"
  return (PrintExp e)

expr = numExp <|> letExp <|> ifExp <|> diffExp <|> isZeroExp <|> 
  printExp <|>
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


valueOf :: Exp -> Env -> IO ExpVal

valueOf (NumExp n) env = return (IntVal n)

valueOf (VarExp var) env = return (applyEnv env var)

valueOf (DiffExp e1 e2) env = do
  val1 <- valueOf e1 env
  val2 <- valueOf e2 env
  return (IntVal ((expValToNum val1) - (expValToNum val2)))

valueOf (IsZeroExp e) env = do
  v <- valueOf e env
  let num = expValToNum v
  if num == 0 
    then return (BoolVal True) 
    else return (BoolVal False)

valueOf (IfExp e1 e2 e3) env = do
  val1 <- valueOf e1 env
  if (expValToBool val1) 
    then (valueOf e2 env) 
    else (valueOf e3 env)

valueOf (LetExp var e1 e2) env = do
  initVal <- valueOf e1 env
  (valueOf e2 (extendEnv var initVal env))

valueOf (PrintExp e) env = do
  val <- valueOf e env
  putStrLn $ "Value: " ++ (show val)
  return val

valueOf' :: String -> Env -> IO ExpVal
valueOf' inp env = case (parse expr inp) of
                     [] -> error "parse error"
                     [(exp, rest)] -> valueOf exp env

sampleEnv = (extendEnv "x" (IntVal 10) (extendEnv "v" (IntVal 5) (extendEnv "i" (IntVal 1) emptyEnv)))
