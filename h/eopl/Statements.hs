module Statements where

import Parser
import Control.Applicative ((<|>), many)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Control.Monad (forM)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)

type Name = String

-- Syntax
newtype Prog = Prog Stmt
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
         | NotExp Exp
  deriving (Show)

prog = stmt

stmt :: Parser Stmt
stmt = 
  assignStmt <|>
  printStmt <|>
  compoundStmt <|> 
  ifStmt <|>
  whileStmt <|>
  declStmt

assignStmt :: Parser Stmt
assignStmt = do
  v <- identifier
  strTok "="
  e <- expr
  return (AssignStmt v e)

printStmt :: Parser Stmt
printStmt = do
  strTok "print"
  e <- expr
  return (PrintStmt e)

compoundStmt :: Parser Stmt
compoundStmt = do
  strTok "{" 
  stmts <- do
    s <- stmt 
    ss <- (many (concatp (strTok ";") stmt))
    return (s:ss)
  strTok "}"
  return (CompoundStmt stmts)

ifStmt :: Parser Stmt
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
  strTok ";"
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
  (unaryOp "not" NotExp) <|>
  varExp

-- Environment
type Env = [(String, ExpVal)]

emptyEnv :: Env
emptyEnv = []

extendEnv :: Name -> ExpVal -> Env -> Env
extendEnv var val env = (var, val):env

extendEnvMult :: [(Name, ExpVal)] -> Env -> Env
extendEnvMult = (++)

applyEnv :: Env -> Name -> Maybe ExpVal
applyEnv env var = L.lookup var env

-- Values
data ExpVal = IntVal Int
            | BoolVal Bool
            | ProcVal String Exp Env
            | RefVal Loc

instance Show ExpVal where
  show (IntVal n) = show n
  show (BoolVal b) = show b
  show (ProcVal name _ _) = "procedure#" ++ name
  show (RefVal loc) = "location#" ++ show loc

expValToNum :: ExpVal -> Int
expValToNum (IntVal n) = n
expValToNum _ = error "number expected"
unboxInt = expValToNum

expValToBool :: ExpVal -> Bool
expValToBool (BoolVal v) = v
expValToBool _ = error "bool expected"
unboxBool = expValToBool

-- Evaluation

type InterpM a = StateT StoreData (WriterT [HistItem] (ExceptT ErrorData IO)) a

evalStmt :: Stmt -> Env -> InterpM ()
evalStmt (AssignStmt var exp) env = do
  case applyEnv env var of
    Just (RefVal loc) -> do 
      val <- evalExp exp env
      setLocVal loc val
      return ()
    Just _ -> throwErr (ErrorData "RefVal expected")
    Nothing -> throwErr (ErrorData "Cannot resolve variable")
evalStmt (PrintStmt exp) env = do
  val <- evalExp exp env
  liftIO (putStrLn (show val))
evalStmt (CompoundStmt stmts) env = do
  forM stmts (\s -> evalStmt s env)
  return ()
evalStmt (IfStmt exp thenS elseS) env = do
  val <- evalExp exp env
  processBoolean val (evalStmt thenS env) (evalStmt elseS env)
evalStmt w@(WhileStmt exp stmt) env = do
  val <- evalExp exp env
  liftIO $ putStrLn (show val)
  processBoolean val (do 
    evalStmt stmt env
    evalStmt w env) 
    (return ())
evalStmt (DeclStmt vars stmt) env = do
  new <- forM vars (\v -> do
    loc <- getNewLoc
    return (v, RefVal loc))
  let newEnv = extendEnvMult new env
  evalStmt stmt newEnv

processBoolean val tbranch fbranch =
  case val of
    (BoolVal True) -> tbranch
    (BoolVal False) -> fbranch
    _ -> throwErr (ErrorData "Bool value expected")

evalExp :: Exp -> Env -> InterpM ExpVal
evalExp (NumExp num) env = 
  return (IntVal num)
evalExp (VarExp var) env = 
  resolveVar var env
evalExp (DiffExp e1 e2) env = do
  val1 <- evalExp e1 env
  val2 <- evalExp e2 env
  return $ IntVal $ (unboxInt val1) - (unboxInt val2)
evalExp (MultExp e1 e2) env = do
  val1 <- evalExp e1 env
  val2 <- evalExp e2 env
  return $ IntVal $ (unboxInt val1) * (unboxInt val2)
evalExp (NotExp e) env = do
  val <- evalExp e env
  return (BoolVal (unboxBool val))
evalExp (IsZeroExp e) env = do
  val <- evalExp e env
  if (unboxInt val) == 0
    then return (BoolVal False)
    else return (BoolVal True)
evalExp e _ = error (show e)

resolveVar :: Name -> Env -> InterpM ExpVal
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
type Loc = Int
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

pns s = do
  res <- rrrun $ evalStmt (fst $ head $ parse stmt s) emptyEnv
  case res of
    Left (ErrorData s) -> putStrLn $ "Error: " ++ s
    Right (_, h) -> do
      putStrLn "History: "
      forM h print
      return ()

main = putStrLn "Hello"

-- Testin
abc = "\
\var f,x; {f = proc(y) *(4,y); \
\x = 3;\
\print (f x)}"

ts1 = "\
\var x,y,z; {x = 6;\
\y = 4;\
\z = 0;\
\while not(zero?(x))\
\{z = -(z,y); x = -(x,1)};\
\print z}"

ex3 = "var x; {x = 3; print x; var x; {x = 4; print x}; print x}"