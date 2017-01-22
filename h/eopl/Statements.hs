module Statements where

import Parser
import Control.Applicative ((<|>), many)
import System.IO
import qualified Data.IntMap as IM
import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)
import Control.Exception.Base (throw)
import Data.Maybe (fromMaybe)

type Name = String

-- Syntax
data Prog = Prog Stmt
  deriving (Show)
data Stmt = AssignStmt Name Exp
          | PrintStmt Exp
          | CompoundStmt [Stmt]
          | IfStmt Exp Stmt Stmt
          | WhileStmt Exp Stmt
          | DeclStmt [Name] Stmt
  deriving (Show)          
data Exp = NumExp Int
         | IsZeroExp Exp
         | VarExp String
         | LetExp String Exp Exp
         | ProcExp String Exp
         | LetRecExp String String Exp Exp
         | CallExp Exp Exp
         | CompoundExp [Exp]
         | DiffExp Exp Exp
         | AddExp Exp Exp
         | MultExp Exp Exp
  deriving (Show)

prog = stmt

stmt = 
  assignStmt <|>
  printStmt <|>
  compoundStmt <|> 
  ifStmt <|>
  whileStmt <|>
  declStmt

assignStmt = do
  v <- identifier
  strTok "="
  e <- expr
  return (AssignStmt v e)

printStmt = do
  strTok "print"
  strTok "("
  e <- expr
  strTok ")"
  return (PrintStmt e)

compoundStmt = do
  strTok "{" 
  stmts <- do
    s <- stmt 
    ss <- (many (concatp (strTok ";") stmt))
    return (s:ss)
  strTok "}"
  return (CompoundStmt stmts)

ifStmt = do
  strTok "if"
  e <- expr
  s1 <- stmt
  s2 <- stmt
  return (IfStmt e s1 s2)

whileStmt = do
  strTok "while"
  e <- expr
  s <- stmt
  return (WhileStmt e s)

declStmt = do
  strTok "var"
  vars <- do
    v <- identifier
    vs <- many (concatp (strTok ",") identifier)
    return (v:vs)
  s <- stmt
  return (DeclStmt vars s)

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
  procExp <|>
  letrec <|>
  callExp <|>
  isZeroExp <|> 
  diffExp <|> 
  (binOp "+" AddExp) <|>
  (binOp "*" MultExp) <|>
  varExp

-- Environment
type Env = [(String, ExpVal)]

extendEnv :: Name -> ExpVal -> Env -> Env
extendEnv var val env = (var, val):env

applyEnv :: Env -> Name -> Maybe ExpVal
applyEnv env var = L.lookup var env

-- Values
data ExpVal = IntVal Int
            | BoolVal Bool
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

-- Expressions
type Loc = Int

type InterpM a = StateT StoreData (WriterT [HistItem] (ExceptT ErrorData IO)) a

evalExp :: Exp -> Env -> InterpM ExpVal
evalExp (NumExp num) env = 
  return (IntVal num)
evalExp (VarExp var) env = 
  resolveVar var env
evalExp (DiffExp e1 e2) env = do
  val1 <- evalExp e1 env
  val2 <- evalExp e2 env
  return $ IntVal $ (unboxInt val1) - (unboxInt val2)
evalExp e _ = error (show e)

resolveVar var env = 
  case applyEnv env var of
    Just (RefVal loc) -> do
      mval <- deref loc
      case mval of
        Just val -> return val
        Nothing -> do
          throwErr (ErrorData "location not found")
    Nothing -> throwErr (ErrorData "variable not found")

-- Errors
data ErrorData = ErrorData String
  deriving (Show)

throwErr :: ErrorData -> InterpM a
throwErr edat = lift $ lift $ throwE edat

-- Storage
type Store = IM.IntMap ExpVal
type StoreData = (Int, Store)

data HistItem = NewRefItem Loc
              | SetRefItem Loc ExpVal
              | DerefItem Loc ExpVal

instance Show HistItem where
  show (NewRefItem n) = "[" ++ (show n) ++ "] new" 
  show (SetRefItem n val) = "[" ++ (show n) ++ "] <- " ++ (show val)
  show (DerefItem n val) = "[" ++ (show n) ++ "] => " ++ (show val)

emptyStor = (0, IM.empty)

getNewLoc :: InterpM Loc
getNewLoc = do
  (n, store) <- get
  let n' = n + 1
  put (n', store)
  tell [NewRefItem $ n']
  return n'

setLocVal :: Loc -> ExpVal -> InterpM ExpVal
setLocVal loc val = do
  (n, store) <- get
  put (n, IM.insert loc val store)
  tell [SetRefItem loc val]
  return val

deref :: Int -> InterpM (Maybe ExpVal)
deref loc = do
  (n, store) <- get
  case IM.lookup loc store of 
    Just val -> do
        put (n, store)
        tell [DerefItem loc val]
        return (Just val)
    Nothing -> return Nothing

newrefAndInit :: ExpVal -> InterpM Loc
newrefAndInit val = do
  loc <- getNewLoc
  setLocVal loc val
  return loc

-- Execution
--pne :: String -> IO (Either ErrorData ExpVal)
rrrun s = runExceptT $ runWriterT $ evalStateT s emptyStor

getHistory s = do
  let x = rrrun s
  ei <- x
  case ei of
    (Left _) -> error "Cannot get nothing"
    (Right (n, h)) -> return h

pne s = do
  v <- runExceptT $ runWriterT $ evalStateT (evalExp (fst $ head (parse expr s)) [])  emptyStor
  print v


main = undefined