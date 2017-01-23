module Statements where

import Parser
import Control.Applicative ((<|>), many)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)

-- Read added
-- Initiliazation is possible in declaration

type Name = String

-- Syntax
newtype Prog = Prog Stmt
            deriving (Show)
data Stmt = AssignStmt Name Exp
          | PrintStmt Exp
          | ReadStmt Name
          | CompoundStmt [Stmt]
          | IfStmt Exp Stmt Stmt
          | WhileStmt Exp Stmt
          | DeclStmt [(Name, Maybe Exp)] Stmt
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
         | IfExp Exp Exp Exp         
  deriving (Show)

prog = stmt

stmt :: Parser Stmt
stmt = 
  assignStmt <|>
  printStmt <|>
  readStmt <|>
  compoundStmt <|> 
  ifStmt <|>
  whileStmt <|>
  declStmt

assignStmt :: Parser Stmt
assignStmt = do
  v <- identifier
  _ <- strTok "="
  e <- expr
  return (AssignStmt v e)

printStmt :: Parser Stmt
printStmt = do
  _ <- strTok "print"
  e <- expr
  return (PrintStmt e)

readStmt :: Parser Stmt
readStmt = do
  _ <- strTok "read"
  v <- identifier
  return (ReadStmt v)

compoundStmt :: Parser Stmt
compoundStmt = do
  _ <- strTok "{" 
  stmts <- do
    s <- stmt 
    ss <- (many (concatp (strTok ";") stmt))
    return (s:ss)
  _ <- strTok "}"
  return (CompoundStmt stmts)

ifStmt :: Parser Stmt
ifStmt = do
  _ <- strTok "if"
  e <- expr
  s1 <- stmt
  s2 <- stmt
  return (IfStmt e s1 s2)

whileStmt = do
  _ <- strTok "while"
  e <- expr
  s <- stmt
  return (WhileStmt e s)

declStmt = do
  _ <- strTok "var"
  vars <- do
    v <- readClause
    vs <- many (concatp (strTok ",") readClause)
    return (v:vs)
  _ <- strTok ";"
  s <- stmt
  return (DeclStmt vars s)
  where readClause = do
          v <- identifier
          do _ <- strTok "="
             e <- expr
             return (v, Just e)
           <|> return (v, Nothing)



numExp :: Parser Exp
numExp = do n <- number
            return (NumExp n)

binOp = binaryOp expr
unaryOp name combine = do _ <- strTok name
                          _ <- strTok "("
                          e <- expr
                          _ <- strTok ")"
                          return (combine e)

diffExp :: Parser Exp
diffExp = binOp "-" DiffExp

isZeroExp :: Parser Exp
isZeroExp = do space
               _ <- strTok "zero?"
               space
               char '('
               e <- expr
               char ')'
               return (IsZeroExp e)

varExp = do v <- identifier
            return (VarExp v)

letExp = do _ <- strTok "let"
            v <- identifier
            _ <- strTok "="
            e1 <- expr
            _ <- strTok "in"
            e2 <- expr
            return (LetExp v e1 e2)

procExp = do _ <- strTok "proc"
             _ <- strTok "("
             var <- identifier
             _ <- strTok ")"
             e <- expr
             return (ProcExp var e)

callExp = do _ <- strTok "("
             e1 <- expr
             e2 <- expr
             _ <- strTok ")"
             return (CallExp e1 e2)

letrec = do _ <- strTok "letrec"
            name <- identifier
            _ <- strTok "("
            arg <- identifier
            _ <- strTok ")"
            _ <- strTok "="
            pbody <- expr
            _ <- strTok "in"
            lbody <- expr
            return (LetRecExp name arg pbody lbody)

ifExp :: Parser Exp
ifExp = do strTok "if"
           e1 <- expr
           strTok "then"
           e2 <- expr
           strTok "else"
           e3 <- expr
           return (IfExp e1 e2 e3)

expr :: Parser Exp
expr = numExp <|> 
  letExp <|> 
  procExp <|>
  letrec <|>
  ifExp <|>
  callExp <|>
  isZeroExp <|> 
  diffExp <|> 
  binOp "+" AddExp <|>
  binOp "*" MultExp <|>
  unaryOp "not" NotExp <|>
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
  val <- evalExp exp env
  setVariableVal var val env
evalStmt (PrintStmt exp) env = do
  val <- evalExp exp env
  liftIO (print val)
evalStmt (ReadStmt var) env = do 
  n <- liftIO readLn
  setVariableVal var (IntVal n) env
  return ()
evalStmt (CompoundStmt stmts) env = do
  forM_ stmts (`evalStmt` env)
  return ()
evalStmt (IfStmt exp thenS elseS) env = do
  val <- evalExp exp env
  processBoolean val (evalStmt thenS env) (evalStmt elseS env)
evalStmt w@(WhileStmt exp stmt) env = do
  val <- evalExp exp env
  liftIO $ print val
  processBoolean val (do 
    evalStmt stmt env
    evalStmt w env) 
    (return ())
evalStmt (DeclStmt vars stmt) env = do
  env' <- extendMultipleDecl vars env
  evalStmt stmt env'

processBoolean val tbranch fbranch =
  case val of
    (BoolVal True) -> tbranch
    (BoolVal False) -> fbranch
    _ -> throwErr (ErrorData "Bool value expected")

extendMultipleDecl :: [(Name, Maybe Exp)] -> Env -> InterpM Env
extendMultipleDecl clauses env = do
  env' <- foldM newLocAndExtend env clauses
  forM_ clauses (evaluate env')
  return env'
  where evaluate _ (var, Nothing) = return ()
        evaluate e (var, Just exp) = do
          val <- evalExp exp e
          setVariableVal var val e
        newLocAndExtend e (var, _) = do
          loc <- getNewLoc
          return (extendEnv var (RefVal loc) e)

evalExp :: Exp -> Env -> InterpM ExpVal
evalExp (NumExp num) env = 
  return (IntVal num)
evalExp (VarExp var) env = 
  resolveVar var env
evalExp (DiffExp e1 e2) env = do
  val1 <- evalExp e1 env
  val2 <- evalExp e2 env
  return $ IntVal $ unboxInt val1 - unboxInt val2
evalExp (MultExp e1 e2) env = do
  val1 <- evalExp e1 env
  val2 <- evalExp e2 env
  return $ IntVal $ unboxInt val1 * unboxInt val2
evalExp (NotExp e) env = do
  val <- evalExp e env
  return (BoolVal (unboxBool val))
evalExp (IsZeroExp e) env = do
  val <- evalExp e env
  case val of
    IntVal 0 -> return (BoolVal True)
    IntVal _ -> return (BoolVal False)
    _ -> throwErr (ErrorData "Expected IntVal")
evalExp (IfExp cond thexp elexp) env = do
  val <- evalExp cond env
  processBoolean val (evalExp thexp env) (evalExp elexp env)
evalExp (ProcExp var body) env = 
  return (ProcVal var body env)
evalExp (CallExp rator rand) env = do
  randVal <- evalExp rand env
  ratorVal <- evalExp rator env
  case ratorVal of
    (ProcVal var body envInner) -> do
      loc <- newrefAndInit randVal
      let extEnv = extendEnv var (RefVal loc) envInner
      evalExp body extEnv
    d -> throwErr (ErrorData ("proc val is expected but was " ++ show d))
evalExp e _ = error (show e)

resolveVar :: Name -> Env -> InterpM ExpVal
resolveVar var env = 
  case applyEnv env var of
    Just (RefVal loc) -> do
      mval <- deref loc
      case mval of
        Just val -> return val
        _ -> throwErr (ErrorData "location not found")
    _ -> throwErr (ErrorData "variable not found")

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
  show (NewRefItem n) = "[" ++ show n ++ "] new" 
  show (SetRefItem n val) = "[" ++ show n ++ "] <- " ++ show val
  show (DerefItem n val) = "[" ++ show n ++ "] => " ++ show val

emptyStor :: StoreData
emptyStor = (0, IM.empty)

getNewLoc :: InterpM Loc
getNewLoc = do
  (n, store) <- get
  let n' = n + 1
  put (n', store)
  tell [NewRefItem n']
  return n'

setLocVal :: Loc -> ExpVal -> InterpM ExpVal
setLocVal loc val = do
  (n, store) <- get
  put (n, IM.insert loc val store)
  tell [SetRefItem loc val]
  return val

setVariableVal :: Name -> ExpVal -> Env -> InterpM ()
setVariableVal var val env = case applyEnv env var of
    Just (RefVal loc) -> do setLocVal loc val
                            return ()
    Just _ -> throwErr (ErrorData "RefVal expected")
    Nothing -> throwErr (ErrorData "Cannot resolve variable")

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
      -- putStrLn "History: "
      -- forM h print
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
ex4 = "var x; {read x; while not(zero?(x)) {x = -(x,1); print x}}"
ex5 = "var f = proc(x) if zero?(x) then -(x,1) else (f -(x,1)); print (f 20)"
ex6 = "var x = 0; print zero?(x)"
ex7 = "var x = 0; print zero?(x)"
ex8 = "var f = proc(x) -((g 1), 200), g = proc(y) y; print (f 10)"
