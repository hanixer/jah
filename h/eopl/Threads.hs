{-# LANGUAGE TypeSynonymInstances #-}
module Threads where 

import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Writer
import Parser 
import Control.Applicative ((<|>), many)
import System.IO
import qualified Data.IntMap as IM
import qualified Data.List as L
import Control.Monad.State

-- Implicit references - variables can store only references to locations with values, but not values
-- Threads added

type Name = String

-- AST             
newtype Program = Program Exp
                deriving (Show)
data Exp = NumExp Int
          | IsZeroExp Exp
          | IfExp Exp Exp Exp
          | VarExp String
          | LetExp String Exp Exp
          | ProcExp [String] Exp
          | LetRecExp String [String] Exp Exp
          | CallExp Exp [Exp]
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
          -- Threads
          | SpawnExp Exp
          -- Other
          | PrintExp Exp
          deriving (Show)

numExp :: Parser Exp
numExp = do n <- number
            return (NumExp n)

binOp :: String -> (Exp -> Exp -> Exp) -> Parser Exp
binOp = binaryOp expr

unaryOp :: String -> (Exp -> Exp) -> Parser Exp
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
               _ <-char '('
               e <- expr
               _ <- char ')'
               return (IsZeroExp e)

ifExp :: Parser Exp
ifExp = do _ <- strTok "if"
           e1 <- expr
           _ <- strTok "then"
           e2 <- expr
           _ <- strTok "else"
           e3 <- expr
           return (IfExp e1 e2 e3)

condPart :: Parser (Exp, Exp)
condPart = do e1 <- expr
              _ <- strTok "==>"
              e2 <- expr
              return (e1, e2)

condExp :: Parser Exp
condExp = do _ <- strTok "cond"
             parts <- many condPart
             _ <- strTok "end"
             return (CondExp parts)

varExp :: Parser Exp
varExp = do v <- identifier
            return (VarExp v)

letExp :: Parser Exp
letExp = do _ <- strTok "let"
            v <- identifier
            _ <- strTok "="
            e1 <- expr
            _ <- strTok "in"
            e2 <- expr
            return (LetExp v e1 e2)

varListExp :: Parser [String]
varListExp = do 
  v <- identifier
  vs <- many (do 
    _ <- strTok ","
    varListExp)
  return (v : concat vs)

argsListExp :: Parser [String]
argsListExp = do 
  _ <- strTok "("
  vs <- varListExp <|> return []
  _ <- strTok ")"
  return vs            

procExp :: Parser Exp
procExp = do 
  _ <- strTok "proc"
  args <- argsListExp
  e <- expr
  return (ProcExp args e)

letrec :: Parser Exp
letrec = do _ <- strTok "letrec"
            name <- identifier
            args <- argsListExp
            _ <- strTok "="
            pbody <- expr
            _ <- strTok "in"
            lbody <- expr
            return (LetRecExp name args pbody lbody)

callExp :: Parser Exp
callExp = do _ <- strTok "("
             e1 <- expr
             es <- many expr
             _ <- strTok ")"
             return (CallExp e1 es)

compoundExp :: Parser Exp
compoundExp = do 
  _ <- strTok "{" <|> strTok "begin"
  e <- expr
  es <- many
          (do _ <- strTok ";"
              expr)
  _ <- strTok "}" <|> strTok "end"
  return (CompoundExp (e : es))

minusExp :: Parser Exp
minusExp = unaryOp "minus" MinusExp

emptyExp :: Parser Exp
emptyExp = do _ <- strTok "empty"
              return EmptyExp

commaSeparExpList :: Parser [Exp]
commaSeparExpList = 
  (do e <- expr
      (do _ <- strTok "," 
          es <- commaSeparExpList
          return (e:es)) 
       <|> return [e]) 
  <|> return []
                   
listExp :: Parser Exp
listExp = do _ <- strTok "list"
             _ <- strTok "("
             es <- commaSeparExpList
             _ <- strTok ")"
             return (ListExp es)

assignExp :: Parser Exp
assignExp = do
  _ <- strTok "set"
  var <- identifier
  _ <- strTok "="
  e <- expr
  return (AssignExp var e)

spawnExp :: Parser Exp
spawnExp = strTok "spawn" >> expr >>= \e -> return $ SpawnExp e

expr :: Parser Exp
expr = numExp <|> letExp <|> ifExp <|> procExp <|> letrec <|> callExp <|>
  compoundExp
  <|> isZeroExp
  <|> minusExp
  <|> diffExp
  <|> binOp "+" AddExp
  <|> binOp "*" MultExp
  <|> binOp "mod" ModExp
  <|> binOp "equal?" EqExp
  <|> binOp "less?" LessExp
  <|> binOp "greater?" GreaterExp
  <|> binOp "cons" ConsExp
  <|> unaryOp "car" CarExp
  <|> unaryOp "cdr" CdrExp
  <|> unaryOp "empty?" IsEmptyExp
  <|> emptyExp
  <|> listExp
  <|> condExp
  <|> unaryOp "print" PrintExp
  <|> unaryOp "newref" NewRefExp
  <|> binOp "setref" SetRefExp
  <|> unaryOp "deref" DerefExp <|>
  assignExp <|>
  spawnExp <|>
  varExp

program :: Parser Program
program = do e <- expr
             return (Program e)

-- Expession values
data ExpVal = IntVal Int
            | BoolVal Bool
            | ConsVal ExpVal ExpVal
            | EmptyVal
            | ProcVal [String] Exp Env
            | RefVal Loc
  deriving (Show)

expValToNum :: ExpVal -> Int
expValToNum (IntVal n) = n
expValToNum _ = error "number expected"
unboxInt :: ExpVal -> Int
unboxInt = expValToNum

expValToBool :: ExpVal -> Bool
expValToBool (BoolVal v) = v
expValToBool _ = error "bool expected"
unboxBool :: ExpVal -> Bool
unboxBool = expValToBool


-- Environment
type Env = [(Name, ExpVal)]

emptyEnv :: Env
emptyEnv = []

extendEnv :: Name -> ExpVal -> Env -> Env
extendEnv var val env = (var, val):env

extendEnvMult :: [(Name, ExpVal)] -> Env -> Env
extendEnvMult = (++)

applyEnv :: Env -> Name -> Maybe ExpVal
applyEnv env var = L.lookup var env

-- Evaluation
type InterpM a = StateT StoreData (WriterT [HistItem] (ExceptT ErrorData IO)) a

eval :: Exp -> Env -> InterpM ExpVal
eval (NumExp n) _ = return (IntVal n)
eval (VarExp name) env = 
  resolveVar name env
eval (DiffExp e1 e2) env = do
  val1 <- eval e1 env
  val2 <- eval e2 env
  return $ IntVal $ unboxInt val1 - unboxInt val2
eval (MultExp e1 e2) env = do
  val1 <- eval e1 env
  val2 <- eval e2 env
  return $ IntVal $ unboxInt val1 * unboxInt val2
eval (LetExp var e1 e2) env = do 
  rhs <- eval e1 env
  loc <- newrefAndInit rhs
  let newEnv = extendEnv var (RefVal loc) env
  eval e2 newEnv
eval (LetRecExp name args pbody body) env = do 
  loc <- getNewLoc
  let env' = extendEnv name (RefVal loc) env
  let procVal = ProcVal args pbody env'
  _ <- setLocVal loc procVal
  eval body env'
eval (ProcExp vars body) env = return $ ProcVal vars body env
eval (CallExp rator rands) envOuter = do
  randVals <- mapM (`eval` envOuter) rands
  ratorVal <- eval rator envOuter
  case ratorVal of
    ProcVal vars body envInner -> do 
      envInner' <- allocateArgs vars randVals envInner
      eval body envInner'
    d -> throwErr (ErrorData $ "proc val is expected but was " ++ show d)
eval (NewRefExp _) _ = throwErr (ErrorData "newref is not available in this language")
eval (SetRefExp _ _) _ = throwErr (ErrorData "setref is not available in this language")
eval (DerefExp _) _ = throwErr (ErrorData "deref is not available in this language")
eval (AssignExp var e) env = do
  val <- eval e env
  case applyEnv env var of
    Just (RefVal loc) -> do
      _ <- setLocVal loc val
      return val
    v -> throwErr (ErrorData ("RefVal is expected but got " ++ show v))
eval (CompoundExp exps) env = do
  vals <- mapM (`eval` env) exps
  return (last vals)
eval (IsZeroExp e) env = do 
  val <- eval e env
  if unboxInt val == 0 
    then return (BoolVal True) 
    else
    return (BoolVal False)
eval (IfExp e1 e2 e3) env = do
  val1 <- eval e1 env
  if unboxBool val1 
  then eval e2 env
  else eval e3 env
eval val _ = error ("not available for " ++ show val)

-- Continuations
type Cont = (ExpVal -> InterpM ())

applyCont :: Cont -> ExpVal -> InterpM ()
applyCont cont = cont

endCont :: Cont
endCont val = do
  liftIO $ putStr "End of computation: "
  liftIO $ print val

zeroCont :: Cont -> Cont
zeroCont cont val = applyCont cont $ BoolVal $ unboxInt val == 0

letExpCont :: Name -> Exp -> Env -> Cont -> Cont
letExpCont var body env cont val =
  evalk body (extendEnv var val env) cont

ifTestCont :: Exp -> Exp -> Env -> Cont -> Cont
ifTestCont thenExp elseExp env cont val =
  if unboxBool val
    then evalk thenExp env cont
    else evalk elseExp env cont

operatorCont :: [Exp] -> Env -> Cont -> Cont 
operatorCont [randExp] env cont ratorVal = 
  evalk randExp env $ operandCont ratorVal cont
operatorCont _ _ _ _ = throwErr (ErrorData "procedure are only with one arg")

operandCont :: ExpVal -> Cont -> Cont
operandCont (ProcVal [arg] body env) cont val =
  evalk body (extendEnv arg val env) cont
operandCont _ _ _ = throwErr (ErrorData "procedure are only with one arg")


evalk :: Exp -> Env -> Cont -> InterpM ()
evalk (VarExp var) env cont = do
  val <- resolveVar var env
  cont val
evalk (NumExp n) _ cont = 
  cont $ IntVal n
evalk (ProcExp vars body) env cont =
  cont $ ProcVal vars body env
evalk (IsZeroExp e) env cont = 
  evalk e env $ zeroCont cont
evalk (IfExp e1 e2 e3) env cont =
  evalk e1 env $ ifTestCont e2 e3 env cont
evalk (LetExp name rhs body) env cont =
  evalk rhs env $ letExpCont name body env cont
evalk (DiffExp e1 e2) env cont =
  evalk e1 env $ \val1 -> 
    evalk e2 env $ \val2 ->
      cont $ IntVal $ unboxInt val1 - unboxInt val2
evalk (CallExp e1 es2) env cont =
  evalk e1 env $ operatorCont es2 env cont
evalk _ _ _ = return ()

resolveVar :: Name -> Env -> InterpM ExpVal
resolveVar var env = 
  case applyEnv env var of
    Just (RefVal loc) -> do
      mval <- deref loc
      case mval of
        Just val -> return val
        Nothing ->
          throwErr (ErrorData "location not found")
    Just val -> return val
    _ -> throwErr (ErrorData $ "variable " ++ var ++ "not found   "  ++ show env)

allocateArgs :: [Name] -> [ExpVal] -> Env -> InterpM Env
allocateArgs vars vals env = 
  foldM (\e (var, val) -> do
    loc <- newrefAndInit val
    return (extendEnv var (RefVal loc) e)) env (zip vars vals)

-- Errors
data ErrorData = ErrorData String
  deriving (Show)

throwErr :: ErrorData -> InterpM a
throwErr edat = lift $ lift $ throwE edat

-- Storage
data HistItem = NewRefItem Loc
              | SetRefItem Loc ExpVal
              | DerefItem Loc ExpVal

instance Show HistItem where
  show (NewRefItem n) = "[" ++ show n ++ "] new" 
  show (SetRefItem n val) = "[" ++ show n ++ "] <- " ++ show val
  show (DerefItem n val) = "[" ++ show n ++ "] => " ++ show val

type Hist = [HistItem]
type StoreData = (Int, IM.IntMap ExpVal)
type Loc = Int

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
  _ <- setLocVal loc val
  return loc

emptyState :: StoreData
emptyState = (0 :: Int, IM.empty)

-- Test helpers

evall :: a
evall = undefined

evalFile :: FilePath -> IO ()
evalFile file = do
  handle <- openFile file ReadMode
  _ <- hGetContents handle
  return ()

fileContents :: FilePath -> IO String
fileContents file = do 
  handle <- openFile file ReadMode
  hGetContents handle

-- showHistoryFile :: FilePath -> IO ()
-- showHistoryFile file = do
--   --contents <- fileContents file
--   --let (_, (_, _, _)) = evall contents
--   print "Storage:\n" 
--   -- print stor
--   -- mapM print
--   --   (reverse hst)

parseFile :: FilePath -> IO ()
parseFile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  print contents
  print $ fst $ head $ parse expr contents

unwrapState
  :: (Monoid w, Monad m) =>
     StateT StoreData (WriterT w (ExceptT e m)) a -> m (Either e (a, w))
unwrapState s = runExceptT $ runWriterT $ evalStateT s emptyState

runk :: String -> IO ()
runk str = do  
  res <- unwrapState (evalk (fst $ head $ parse expr str) emptyEnv endCont)
  case res of
    Left (ErrorData errr) -> print errr
    Right (_, h) -> do
      mapM_ print h
      return ()
  
run :: String -> IO ()
run str = do  
  res <- unwrapState (eval (fst $ head $ parse expr str) emptyEnv)
  case res of
    Left (ErrorData err) -> putStrLn ("Error: " ++ err)
    Right (expres, _) ->
      print expres

runFile :: FilePath -> IO ()
runFile file = do
  contents <- fileContents file
  run contents

exs :: [String]
exs = [
  "let x = 200 in let f = proc (z) -(z,x) in let x = 100 in let g = proc (z) -(z,x) in -((f 1), (g 1))",
  "let f = proc(x,y) -(x,*(y,-(x,2))) in (f 5 6)",
  "letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 22)",
  "let p = proc(x) set x = 4 in let a = 3 in {(p a);a}"
  ]

exl = last exs


