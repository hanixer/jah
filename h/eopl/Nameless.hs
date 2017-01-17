{-# LANGUAGE TypeSynonymInstances #-}
module Letrec2 where 

import Parser 
import Control.Applicative ((<|>), many)
import System.IO

-- Nameless: here added three rules:
--   - nameless var exp
--   - nameless let exp
--   - nameless proc exp
-- Added translator that translates AST from original to nameless syntax.
-- Interpreter handles only namesless syntax.
-- Added nameless environment

-- AST             
data Program = Program Exp
  deriving (Show)
data Exp = NumExp Int
          | IsZeroExp Exp
          | IfExp Exp Exp Exp
          | VarExp String
          | LetExp String Exp Exp
          | ProcExp String Exp
          | LetRecExp String String Exp Exp
          | CallExp Exp Exp
          | NamelessVarExp Lexaddr
          | NamelessLetExp Exp Exp
          | NamelessProcExp Exp
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
          -- Other
          | PrintExp Exp
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

procExp = do strTok "proc"
             strTok "("
             var <- identifier
             strTok ")"
             e <- expr
             return (ProcExp var e)

callExp = do strTok "("
             e1 <- expr
             e2 <- expr
             strTok ")"
             return (CallExp e1 e2)

minusExp = unaryOp "minus" MinusExp

emptyExp = do strTok "empty"
              return (EmptyExp)

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

letrec = do strTok "letrec"
            name <- identifier
            strTok "("
            arg <- identifier
            strTok ")"
            strTok "="
            pbody <- expr
            strTok "in"
            lbody <- expr
            return (LetRecExp name arg pbody lbody)

expr = numExp <|> 
  letExp <|> 
  ifExp <|> 
  procExp <|>
  letrec <|>
  callExp <|>
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
  condExp <|>
  (unaryOp "print" PrintExp) <|>
  varExp

program = do e <- expr
             return (Program e)

-- Expession values
data ExpVal = IntVal Int
            | BoolVal Bool
            | ConsVal ExpVal ExpVal
            | EmptyVal
            | ProcVal String Exp Env
            | NamelessProcVal Exp NamelessEnv
  deriving (Show)

expValToNum :: ExpVal -> Int
expValToNum (IntVal n) = n
expValToNum _ = error "number expected"
unboxInt = expValToNum

expValToBool :: ExpVal -> Bool
expValToBool (BoolVal v) = v
expValToBool _ = error "bool expected"
unboxBool = expValToBool

{-
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
-}

-- Environment
newtype Env = Env {envFunc :: String -> Lexaddr -> (ExpVal, Lexaddr)}
type NamelessEnv = [ExpVal]
type Lexaddr = Int

instance Show Env where
  show _ = "Environment"

emptyEnv :: Env
emptyEnv = Env (\_ _ -> error "variable not found")

extendEnv :: String -> ExpVal -> Env -> Env
extendEnv var val env = Env (\v addr ->
  if v == var 
  then (val, addr) 
  else envFunc env v (addr + 1))

extendEnvRec :: String -> String -> Exp -> Env -> Env
extendEnvRec name arg body env = Env (\v addr ->
  if v == name 
  then ((ProcVal arg body (extendEnvRec name arg body env)), addr)
  else envFunc env v (addr + 1))

extendEnvMany [] env = env
extendEnvMany ((var,val):rest) env = 
  extendEnvMany rest $ extendEnv var val env

applyEnv :: Env -> String -> ExpVal
applyEnv env searchVar = fst $ envFunc env searchVar 0

lexaddr :: Env -> String -> Lexaddr
lexaddr env searchVar = snd $ envFunc env searchVar 0

emptyNamelessEnv :: NamelessEnv
emptyNamelessEnv = []

extendNamelessEnv :: ExpVal -> NamelessEnv -> NamelessEnv
extendNamelessEnv val env  = (val:env)

applyNamelessEnv :: NamelessEnv -> Lexaddr -> ExpVal
applyNamelessEnv env num = env !! num

-- Translation to nameless AST
translationOf :: Exp -> Env -> Exp
translationOf (VarExp var) env = NamelessVarExp (lexaddr env var)
translationOf (LetExp var init body) env = 
  NamelessLetExp (translationOf init env) (translationOf body (extendEnv var (IntVal 1) env ))
translationOf (ProcExp arg body) env = NamelessProcExp (translationOf body (extendEnv arg (IntVal 1) env ))
translationOf (IsZeroExp e) env = IsZeroExp $ translationOf e env
translationOf (MinusExp e) env = MinusExp $ translationOf e env
translationOf (DiffExp e1 e2) env = DiffExp (translationOf e1 env) (translationOf e2 env)
translationOf (CallExp e1 e2) env = CallExp (translationOf e1 env) (translationOf e2 env)
translationOf exp _ = exp

-- Evaluation
valueOf :: Exp -> NamelessEnv -> ExpVal

valueOf (NumExp n) _ = (IntVal n)

valueOf (NamelessVarExp addr) env = applyNamelessEnv env addr

valueOf (IsZeroExp e) env = if num == 0 then (BoolVal True) else (BoolVal False)
  where num = (expValToNum (valueOf e env))

valueOf (IfExp e1 e2 e3) env = if val1 then (valueOf e2 env) else (valueOf e3 env)
  where val1 = (expValToBool (valueOf e1 env))

valueOf (NamelessLetExp init body) env = 
  valueOf body (extendNamelessEnv (valueOf init env) env)

valueOf (MinusExp e) env = (IntVal (-(expValToNum (valueOf e env))))
valueOf (DiffExp e1 e2) env = 
  IntVal $ (unboxInt $ valueOf e1 env) - (unboxInt $ valueOf e2 env)

{-

valueOf (AddExp e1 e2) env = extractAndApply e1 e2 env (+)
valueOf (MultExp e1 e2) env = extractAndApply e1 e2 env (*)
valueOf (ModExp e1 e2) env = binOpImplInt e1 e2 env mod 
valueOf (EqExp e1 e2) env = relOpImpl e1 e2 env (==) 
valueOf (GreaterExp e1 e2) env = relOpImpl e1 e2 env (>) 
valueOf (LessExp e1 e2) env = relOpImpl e1 e2 env (<) 
-}
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

valueOf (CondExp exps) env = eval exps
  where eval [] = error "no valid cond clause found"
        eval ((cond,body):exps) = 
          if (unboxBool (valueOf cond env)) 
          then valueOf body env
          else eval exps

valueOf (NamelessProcExp body) env = (NamelessProcVal body env)
--valueOf (LetRecExp name arg pbody lbody) env = 
--  valueOf lbody $ (extendEnvRec name arg pbody env)
valueOf (CallExp rator rand) envOuter =  
  case valueOf rator envOuter of 
    (NamelessProcVal body envInner) -> 
      let randVal = valueOf rand envOuter
      in valueOf body (extendNamelessEnv randVal envInner) 
    _ -> error "operator must be procedure value"

valueOf _ _ = error "not implemented for this syntax"

-- Test helpers
valueOf' :: String -> Env -> ExpVal
valueOf' inp env = case (parse expr inp) of
                     [] -> error "parse error"
                     [(exp, rest)] -> valueOf exp emptyNamelessEnv

sampleEnv = (extendEnv "x" (IntVal 10) (extendEnv "v" (IntVal 5) (extendEnv "i" (IntVal 1) emptyEnv)))

eval s = valueOf' s sampleEnv

evalNl s = valueOf (translationOf (fst . head $ parse expr s) emptyEnv) emptyNamelessEnv

evalFile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  print $ evalNl contents

trlt s = (translationOf (fst . head $ parse expr s) emptyEnv)

s1 = "let x = 200 in let f = proc (z) -(z,x) in let x = 100 in let g = proc (z) -(z,x) in -((f 1), (g 1))"
multi = "line1\
\line2\
\line3"
{-
s2 = "\
let a = 1 in\\n\
let b = +(a,1) in\\n\
let f = proc(x) *(a,b) in\\n\
f(100)"

-}
