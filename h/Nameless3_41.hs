{-# LANGUAGE TypeSynonymInstances #-}
module Nameless3_41 where 

import Parser 
import Control.Applicative ((<|>), many)
import System.IO
import Data.List (find)
-- Nameless: hereadded three rules:
--   - nameless var exp
--   - nameless let exp
--   - nameless proc exp
-- Added translator that translates AST from original to nameless syntax.
-- Interpreter handles only namesless syntax.
-- Added nameless environment
-- ribcage environment representation
-- multiple-argument procedures

-- AST             
data Program = Program Exp
  deriving (Show)
data Exp = NumExp Int
          | IsZeroExp Exp
          | IfExp Exp Exp Exp
          | VarExp String
          | LetExp String Exp Exp
          | ProcExp [String] Exp
          | CallExp Exp [Exp]
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
          | UnpackExp [String] Exp Exp
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

varListExp elem = do  
  v <- elem
  vs <- (many (do strTok ","
                  vs' <- (varListExp elem)
                  return vs'))
  return (v:(concat vs))

argsListExp elem = do 
  strTok "("
  vs <- ((varListExp elem) <|> return [])
  strTok ")"
  return vs

procExp = do strTok "proc"
             args <- (argsListExp identifier)
             e <- expr
             return (ProcExp args e)

callExp = do 
  strTok "("
  procExp <- expr
  params <- (many expr)
  strTok ")"
  return (CallExp procExp params)

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
             return $ expListToCons es

expListToCons expList =
  foldl (\cons exp -> ConsExp exp cons) EmptyExp $ reverse expList

unpackExp = 
  do strTok "unpack"
     vs <- (oneAndMore identifier id)
     strTok "="
     e1 <- expr
     strTok "in"
     e2 <- expr
     return (UnpackExp vs e1 e2)

expr = numExp <|> 
  letExp <|> 
  ifExp <|> 
  procExp <|>
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
  unpackExp <|>
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
expValToBool (IntVal n) = if n == 0 then False else True
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
type Env = [([String],[ExpVal])]
emptyEnv :: Env
emptyEnv = []

extendEnv :: String -> ExpVal -> Env -> Env
extendEnv var val env = (([var],[val]):env)

extendEnv' :: [String] -> Env -> Env
extendEnv' vars env = ((vars,take (length vars) (repeat EmptyVal)):env)

lexaddr :: Env -> String -> Lexaddr
lexaddr env name = lexaddr' (zip env [0..])
  where lexaddr' [] = error "variable not found"
        lexaddr' ((x,i):xs) = case lexaddrpos x name of
          Nothing -> lexaddr' xs
          Just j -> (i,j)

lexaddrpos :: ([String],[ExpVal]) -> String -> Maybe Int
lexaddrpos (vars,vals) name = 
  case find (\(var,j) -> name == var) (zip vars [0..]) of
    Nothing -> Nothing
    Just (_,j) -> Just j

type NamelessEnvEntry = [ExpVal]
type NamelessEnv = [NamelessEnvEntry]
type Lexaddr = (Int,Int)

emptyNamelessEnv :: NamelessEnv
emptyNamelessEnv = []

extendNamelessEnv :: ExpVal -> NamelessEnv -> NamelessEnv
extendNamelessEnv val env  = ([val]:env)

extendNamelessEnv' :: [ExpVal] -> NamelessEnv -> NamelessEnv
extendNamelessEnv' vals env  = (vals:env)

applyNamelessEnv :: NamelessEnv -> Lexaddr -> ExpVal
applyNamelessEnv env (i,j) = env !! i !! j
  
extendNamelessEnvUnpack :: [String] -> ExpVal -> NamelessEnv -> NamelessEnv
extendNamelessEnvUnpack [] _ env = env
extendNamelessEnvUnpack (x:_) EmptyVal env = 
  error "not enough cons cells for unpack"
extendNamelessEnvUnpack (x:xs) (ConsVal e rest) env = 
  extendNamelessEnvUnpack xs rest (extendNamelessEnv e env)
extendNamelessEnvUnpack (x:_) _ _ = error "cons value is expected"

-- Translation to nameless AST
trans :: Exp -> Env -> Exp
trans (VarExp var) env = 
  NamelessVarExp $ lexaddr env var
trans (LetExp var init body) env = 
  NamelessLetExp
  (trans init env)
  (trans body (extendEnv' [var] env))
trans (ProcExp args body) env = 
  NamelessProcExp (trans body (extendEnv' args env))
trans (IfExp e1 e2 e3) env = 
  IfExp (trans e1 env) (trans e2 env) (trans e3 env)
trans (IsZeroExp e) env = 
  IsZeroExp $ trans e env
trans (MinusExp e) env = 
  MinusExp $ trans e env
trans (DiffExp e1 e2) env = 
  DiffExp (trans e1 env) (trans e2 env)
trans (CallExp rator rands) env = 
  CallExp (trans rator env) (map (\e -> trans e env) rands)
trans (CondExp clauses) env = 
  CondExp $ map (\(a,b) -> 
                   ((trans a env), trans b env)) clauses
trans (ConsExp e1 e2) env = 
  ConsExp (trans e1 env) (trans e2 env)
trans (UnpackExp names rhs body) env = 
  trans (unpackToLet names rhs body) env
trans exp _ = exp

unpackToLet (x:xs) (ConsExp car cdr) body =
  LetExp x car (unpackToLet xs cdr body)
unpackToLet [] _ body = body
unpackToLet (_:_) _ _ = error "cons expect on the rhs of unpack"

-- Evaluation
valueOf :: Exp -> NamelessEnv -> ExpVal

valueOf (NumExp n) _ = (IntVal n)

valueOf (NamelessVarExp addr) env = applyNamelessEnv env addr

valueOf (NamelessProcExp body) env = (NamelessProcVal body env)
valueOf (CallExp rator rands) envOuter =  
  case valueOf rator envOuter of 
    (NamelessProcVal body envInner) -> 
      let randVals = map (\e -> valueOf e envOuter) rands
      in valueOf body (extendNamelessEnv' randVals envInner) 
    _ -> error "operator must be procedure value"

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
valueOf (UnpackExp vs rhs body) env = 
  valueOf body (extendNamelessEnvUnpack vs (valueOf rhs env) env)
valueOf (CondExp exps) env = eval exps
  where eval [] = error "no valid cond clause found"
        eval ((cond,body):exps) = 
          if (unboxBool (valueOf cond env)) 
          then valueOf body env
          else eval exps
valueOf _ _ = error "not implemented for this syntax"

-- Test helpers
valueOf' :: String -> Env -> ExpVal
valueOf' inp env = case (parse expr inp) of
                     [] -> error "parse error"
                     [(exp, rest)] -> valueOf exp emptyNamelessEnv

sampleEnv = extendEnv' ["j"] $ extendEnv' ["a","b"] $ extendEnv' ["x","y","z"] emptyEnv
sampleNlEnv = extendNamelessEnv' [(IntVal 10050)] $ extendNamelessEnv' [(IntVal 1),EmptyVal,EmptyVal] $ extendNamelessEnv' [(IntVal 1),EmptyVal,EmptyVal] emptyNamelessEnv
eval s = valueOf' s emptyEnv

evalNl s = valueOf (trans (fst . head $ parse expr s) emptyEnv) emptyNamelessEnv

evalFile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  print $ evalNl contents

trlt s = (trans (fst . head $ parse expr s) emptyEnv)

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
