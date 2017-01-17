{-# LANGUAGE TypeSynonymInstances #-}
module Explref where 

import Parser 
import Control.Applicative ((<|>), many)
import System.IO
import qualified Data.IntMap as IM
import Control.Monad.State
import Data.Maybe (fromMaybe)

-- Implicit references - variables can store only references to locations with values, but not values

-- AST             
data Program = Program Exp
  deriving (Show)
data Exp = NumExp Int
          | IsZeroExp Exp
          | IfExp Exp Exp Exp
          | VarExp String
          | LetExp String Exp Exp
          | ProcExp String Exp
          | LetRecExp [(String, String, Exp)] Exp
          | CallExp Exp Exp
          | CompoundExp [Exp]
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
          -- Storage
          | NewRefExp Exp
          | SetRefExp Exp Exp
          | DerefExp Exp
          | AssignExp String Exp
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

compoundExp = do
  (strTok "{") <|> (strTok "begin")
  e <- expr
  es <- many (do { strTok ";"; expr })
  (strTok "}") <|> (strTok "end")
  return (CompoundExp (e:es))

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
            fs <- oneAndMore (do                           
              name <- identifier
              strTok "("
              arg <- identifier
              strTok ")"
              strTok "="
              pbody <- expr
              return (name, arg, pbody)) id
            strTok "in"
            lbody <- expr
            return (LetRecExp fs lbody)

assignExp = do
  strTok "set"
  var <- identifier
  strTok "="
  exp <- expr
  return (AssignExp var exp)

expr = numExp <|> 
  letExp <|> 
  ifExp <|> 
  procExp <|>
  letrec <|>
  callExp <|>
  compoundExp <|>
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
  (unaryOp "newref" NewRefExp) <|>
  (binOp "setref" SetRefExp) <|>
  (unaryOp "deref" DerefExp) <|>
  assignExp <|>
  varExp

program = do e <- expr
             return (Program e)

-- Expession values
data ExpVal = IntVal Int
            | BoolVal Bool
            | ConsVal ExpVal ExpVal
            | EmptyVal
            | ProcVal String Exp Env
            | RefVal Loc
  deriving (Show)

expValToNum :: ExpVal -> Int
expValToNum (IntVal n) = n
expValToNum _ = error "number expected"
unboxInt = expValToNum

expValToBool :: ExpVal -> Bool
expValToBool (BoolVal v) = v
expValToBool _ = error "bool expected"
unboxBool = expValToBool


-- Environment
data EnvEntry = SimpleEnvEntry String ExpVal
              | RecEnvEntry String ExpVal
              deriving (Show)
type Env = [EnvEntry]

emptyEnv :: Env
emptyEnv = []

extendEnv :: String -> ExpVal -> Env -> State StoreState Env
extendEnv var val env = return (SimpleEnvEntry var val:env)

extendEnvRecProcs procs env = 
  foldM (\env (name, arg, exp) -> do
            loc <- getNewLoc
            env' <- extendEnv name (RefVal loc) env
            setLocVal loc (ProcVal arg exp env')
            return env') env procs

applyEnv :: Env -> String -> State StoreState ExpVal
applyEnv (SimpleEnvEntry var val:env) svar = 
  if var == svar 
  then return val 
  else applyEnv env svar
applyEnv [] var = error ("variable " ++ (show var) ++ " not found")


-- Evaluation
eval :: Exp -> Env -> State StoreState ExpVal
eval (NumExp n) env = return (IntVal n)
eval (VarExp name) env = do
  val <- applyEnv env name
  deref val
eval (DiffExp e1 e2) env = do
  val1 <- eval e1 env
  val2 <- eval e2 env
  return $ IntVal $ (unboxInt val1) - (unboxInt val2)
eval (LetExp var e1 e2) env = do 
  rhs <- eval e1 env
  loc <- getNewLoc
  setLocVal loc rhs
  newEnv <- extendEnv var (RefVal loc) env
  val <- eval e2 newEnv
  return val
eval (LetRecExp procs body) env = do
  env' <- extendEnvRecProcs procs env
  eval body env'
{-
eval (LetRecExp name arg pbody body) env = do
  let procVal = (ProcVal arg pbody env)
  loc <- getNewLoc
  setLocVal loc procVal
  val <- eval body (extendEnvRec name (ProcVal arg pbody env)  env)
  return val  
-}
eval (ProcExp var body) env = return $ ProcVal var body env
eval (CallExp rator rand) envOuter = do
  randVal <- eval rand envOuter
  ratorVal <- eval rator envOuter
  case ratorVal of
    (ProcVal var body envInner) -> do
      loc <- getNewLoc
      setLocVal loc randVal
      extEnv <- extendEnv var (RefVal loc) envInner
      bodyVal <- eval body extEnv
      return bodyVal
    d -> error ("proc val is expected but was " ++ (show d))
eval (NewRefExp exp) env = error "newref is not available in this language"
eval (SetRefExp exp1 exp2) env = error "setref is not available in this language"
eval (DerefExp exp) env = error "deref is not available in this language"
eval (AssignExp var exp) env = do
  val <- eval exp env
  ref <- applyEnv env var
  case ref of
    (RefVal loc) -> do
      setLocVal loc val
      return val
    v -> error ("RefVal is expected but got " ++ (show v))
eval (CompoundExp exps) env = do
  vals <- mapM (\exp -> eval exp env) exps
  return (head (reverse vals))
eval (IsZeroExp exp) env = do
  val <- eval exp env
  resVal <- if (unboxInt val) == 0
            then return (BoolVal True) 
            else return (BoolVal False)
  return resVal
eval (IfExp e1 e2 e3) env = do
  val1 <- eval e1 env
  if unboxBool val1 
  then eval e2 env
  else eval e3 env
eval val _ = error ("not available for " ++ (show val))

-- Storage
data HistItem = NewRefItem Loc
              | SetRefItem Loc ExpVal
              | DerefItem Loc ExpVal

instance Show HistItem where
  show (NewRefItem n) = "[" ++ (show n) ++ "] new" 
  show (SetRefItem n val) = "[" ++ (show n) ++ "] <- " ++ (show val)
  show (DerefItem n val) = "[" ++ (show n) ++ "] => " ++ (show val)

type Hist = [HistItem]
type Store = IM.IntMap ExpVal
type StoreState = (Int, Store, Hist)
type Loc = Int

getNewLoc :: State StoreState Loc
getNewLoc = do
  (n, store, hst) <- get
  put (n+1, store, (NewRefItem (n + 1)):hst)
  return (n + 1)

setLocVal :: Loc -> ExpVal -> State StoreState ExpVal
setLocVal loc val = do
  (n, store, hst) <- get
  put (n, IM.insert loc val store, (SetRefItem loc val):hst)
  return val

deref :: ExpVal -> State StoreState ExpVal
deref (RefVal loc) = do
  (n, store, hst) <- get
  case IM.lookup loc store of 
    Just val -> do
        put (n, store, (DerefItem loc val):hst)
        return val
    Nothing -> error "no value for this location "
deref _ = error "RefVal expected at deref"

newrefAndInit val = do
  loc <- getNewLoc
  setLocVal loc val
  return loc

emptyState = (0 :: Int, IM.empty, [])
-- Test helpers

--sampleEnv = (extendEnv "x" (IntVal 10) (extendEnv "v" (IntVal 5) (extendEnv "i" (IntVal 1) emptyEnv)))

evall s = runState (eval (fst $ head $ parse expr s) emptyEnv) emptyState


evalFile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  print $ fst $ evall contents

fileContents file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  return contents

showHistoryFile file = do
  contents <- fileContents file
  let (_, (_, stor, hst)) = evall contents
  print "Storage:\n" 
  print stor
  mapM (\item -> print item)
    (reverse hst)

parseFile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  print contents
  print $ fst $ head $ parse expr contents

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
