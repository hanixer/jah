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
          | AddExp Exp Exp
          | MultExp Exp Exp
          | ModExp Exp Exp
          | EqExp Exp Exp
          | GreaterExp Exp Exp
          | LessExp Exp Exp
          deriving (Show)

numExp :: Parser Exp
numExp = do n <- number
            return (NumExp n)

binOp = binaryOp expr

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

varExp = do v <- identifier
            return (VarExp v)

letExp = do strTok "let"
            v <- identifier
            strTok "="
            e1 <- expr
            strTok "in"
            e2 <- expr
            return (LetExp v e1 e2)

minusExp = do strTok "minus"
              strTok "("
              e <- expr
              strTok ")"
              return (MinusExp e)

--addExp = binary
            

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

valueOfAndToNum :: Exp -> Env -> Int
valueOfAndToNum e env = expValToNum (valueOf e env)

--extractAndApply' :: (c -> ExpVal) ->
                    --Exp -> Exp -> Env -> 
                    --(a -> b -> c) ->                     
                    --ExpVal
--extractAndApply' boxer e1 e2 env f  = 
--  boxer (f (valueOfAndToNum e1 env)
          --(valueOfAndToNum e2 env))
  
extractAndApply e1 e2 env f =   IntVal (f (valueOfAndToNum e1 env)
           (valueOfAndToNum e2 env))

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
valueOf (ModExp e1 e2) env = extractAndApply e1 e2 env mod
--valueOf (

valueOf' :: String -> Env -> ExpVal
valueOf' inp env = case (parse expr inp) of
                     [] -> error "parse error"
                     [(exp, rest)] -> valueOf exp env

sampleEnv = (extendEnv "x" (IntVal 10) (extendEnv "v" (IntVal 5) (extendEnv "i" (IntVal 1) emptyEnv)))
