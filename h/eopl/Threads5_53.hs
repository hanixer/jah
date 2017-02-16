{-# LANGUAGE TypeSynonymInstances #-}
module Threads where 
--import Control.Monad (void)
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
-- Thread identifiers added

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
          | MutexExp
          | WaitExp Exp
          | SignalExp Exp
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
spawnExp = do
  _ <- strTok "spawn"
  _ <- strTok "(" 
  e <- expr
  _ <- strTok ")" 
  return $ SpawnExp e

mutextExp :: Parser Exp
mutextExp = do
  _ <- strTok "mutex"
  _ <- strTok "(" 
  _ <- strTok ")" 
  return MutexExp

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
  mutextExp <|>
  unaryOp "wait" WaitExp <|>
  unaryOp "signal" SignalExp <|>
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
            | MutexVal Bool [Thread]
            | MutexRef Loc

instance Show ExpVal where
  show (IntVal n) = "<" ++ show n ++ ">"
  show (BoolVal b) = "<" ++ show b ++ ">"
  show (RefVal loc) = "<loc:" ++ show loc ++ ">"
  show (ProcVal vs _ _) = "<proc:" ++ show vs ++ ">"
  show EmptyVal = "<empty>"
  show (MutexVal b ts) = "<mutex: " ++ show b ++ ", " ++ show (length ts) ++ ">"
  show _ = "<otherval>"


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
type InterpM a = StateT StateData (WriterT [HistItem] (ExceptT ErrorData IO)) a

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
type Cont = (ExpVal -> InterpM ExpVal)

applyCont :: Cont -> ExpVal -> InterpM ExpVal
applyCont cont val = do
  -- liftIO $ putStrLn $ show val ++ "\n--------------------\n"
  timeExp <- isTimeExpired
  if timeExp
    then placeThreadOnQueue (\_ -> applyCont cont val) >> runNextThread
    else decrementTicks >> cont val

endMainThreadCont :: Cont
endMainThreadCont val =  
  setFinalAnswer val >> return EmptyVal >> runNextThread

endOtherThreadCont :: Cont
endOtherThreadCont _ = runNextThread

endCont :: Cont
endCont val = do
  liftIO $ putStr "End of computation: "
  liftIO $ print val
  return EmptyVal

zeroCont :: Cont -> Cont
zeroCont cont val = applyCont cont $ BoolVal $ unboxInt val == 0

letExpCont :: Name -> Exp -> Env -> Cont -> Cont
letExpCont var body env cont val = do
  env' <- allocateArgs [var] [val] env
  evalk body env' cont

ifTestCont :: Exp -> Exp -> Env -> Cont -> Cont
ifTestCont thenExp elseExp env cont val =
  if unboxBool val
    then evalk thenExp env cont
    else evalk elseExp env cont

operatorCont :: [Exp] -> Env -> Cont -> Cont 
operatorCont [] _ cont (ProcVal _ body env) =
  evalk body env cont
operatorCont (e:es) env cont ratorVal =
  evalk e env $ operandCont ratorVal es [] env cont
operatorCont _ _ _ _ = throwErr (ErrorData "operatorCont error")

operandCont :: ExpVal -> [Exp] -> [ExpVal] -> Env -> Cont -> Cont
operandCont (ProcVal args body env) [] randVals _ cont randVal = do
  env' <- allocateArgs args (reverse (randVal:randVals)) env
  evalk body env' cont
operandCont procVal (e:es) randVals env cont val = 
  evalk e env $ operandCont procVal es (val:randVals) env cont
operandCont _ _ _ _ _ _ = throwErr (ErrorData "operandCont error")

diffCont1 :: Exp -> Env -> Cont -> Cont
diffCont1 e2 env cont val1 = 
  evalk e2 env (diffCont2 val1 cont)

diffCont2 :: ExpVal -> Cont -> Cont
diffCont2 val1 cont val2 = 
  applyCont cont (IntVal $ unboxInt val1 - unboxInt val2)

compoundCont :: [Exp] -> Env -> Cont -> Cont
compoundCont [] _ cont val = cont val
compoundCont (e:es) env cont _ = 
  evalk e env $ compoundCont es env cont

assignCont :: String -> Env -> Cont -> Cont
assignCont var env cont val = case applyEnv env var of
  Just (RefVal loc) -> 
    setLocVal loc val >> return val >> applyCont cont val
  v -> throwErr (ErrorData ("RefVal is expected but got " ++ show v ++ "value is " ++ show val)) >>
    applyCont cont val

spawnCont :: Cont -> Cont
spawnCont cont (ProcVal args body env) = do
  env' <- allocateArgs args (replicate (length args) EmptyVal) env
  placeThreadOnQueue (\_ -> evalk body env' endOtherThreadCont)
  applyCont cont EmptyVal
spawnCont _ e = throwErr (ErrorData $ "Procval expected but got " ++ show e)

printCont :: Cont -> Cont
printCont cont val = do
  liftIO $ putStr "Program output: "
  liftIO $ print val
  applyCont cont EmptyVal

waitCont :: Cont -> Cont
waitCont cont (MutexRef loc) = do
  opened <- isMutexOpened loc
  if opened
    then openMutex loc False >> applyCont cont EmptyVal
    else placeThreadOnMutexQueue loc (\_ -> applyCont cont EmptyVal) >> runNextThread
waitCont _ _ = throwErr (ErrorData "mutex ref expected")

signalCont :: Cont -> Cont
signalCont cont (MutexRef loc) = do
  opened <- isMutexOpened loc
  empt <- isMutexQueueEmpty loc
  if opened
    then applyCont cont EmptyVal
    else if empt
      then openMutex loc True >> applyCont cont EmptyVal
      else scheduleMutexThread loc >> applyCont cont EmptyVal

signalCont _ _ = throwErr (ErrorData "mutex ref expected")

evalk :: Exp -> Env -> Cont -> InterpM ExpVal
evalk (VarExp var) env cont = do
  val <- resolveVar var env
  applyCont cont val
evalk (NumExp n) _ cont = 
  applyCont cont $ IntVal n
evalk (ProcExp vars body) env cont =
  applyCont cont $ ProcVal vars body env
evalk (IsZeroExp e) env cont = 
  evalk e env $ zeroCont cont
evalk (IfExp e1 e2 e3) env cont =
  evalk e1 env $ ifTestCont e2 e3 env cont
evalk (LetExp name rhs body) env cont =
  evalk rhs env $ letExpCont name body env cont
evalk (DiffExp e1 e2) env cont =
  evalk e1 env (diffCont1 e2 env cont) 
evalk (CallExp e1 es2) env cont =
  evalk e1 env $ operatorCont es2 env cont
evalk (CompoundExp (e:es)) env cont =
  evalk e env $ compoundCont es env cont
evalk (AssignExp var e) env cont = 
  evalk e env $ assignCont var env cont
evalk (SpawnExp e) env cont = 
  evalk e env $ spawnCont cont
evalk (LetRecExp name args pbody body) env cont = do
  loc <- getNewLoc
  let env' = extendEnv name (RefVal loc) env
  let procVal = ProcVal args pbody env'
  _ <- setLocVal loc procVal
  evalk body env' cont
evalk (PrintExp e) env cont = 
  evalk e env $ printCont cont
evalk MutexExp _ cont = do
  mx <- newMutex
  applyCont cont mx
evalk (WaitExp e) env cont =
  evalk e env $ waitCont cont
evalk (SignalExp e) env cont = 
  evalk e env $ signalCont cont
evalk e _ _ = throwErr $ ErrorData $ "Evalk error " ++ show e

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
type Store = (Int, IM.IntMap ExpVal)
type Loc = Int
data StateData = StateData 
  { store :: Store
  , maxTicks :: Int
  , currTicks :: Int
  , threadQueue :: [TId]
  , currTId :: TId
  , threads :: ThreadStorage
  , mutexes :: MutexStorage
  , finalAnswer :: ExpVal }

getNewLoc :: InterpM Loc
getNewLoc = do
  sdata <- get
  let (n, memry) = store sdata
  let n' = n + 1
  put $ sdata {store = (n', memry)}
  tell [NewRefItem n']
  return n'

setLocVal :: Loc -> ExpVal -> InterpM ExpVal
setLocVal loc val = do
  sdata <- get
  let (n, memry) = store sdata
  put $ sdata {store = (n, IM.insert loc val memry)}
  tell [SetRefItem loc val]
  return val

deref :: Int -> InterpM (Maybe ExpVal)
deref loc = do
  sdata <- get
  let (n, memry) = store sdata
  case IM.lookup loc memry of 
    Just val -> do
      put $ sdata {store = (n, memry)}
      tell [DerefItem loc val]
      return (Just val)
    Nothing -> return Nothing

newrefAndInit :: ExpVal -> InterpM Loc
newrefAndInit val = do
  loc <- getNewLoc
  _ <- setLocVal loc val
  return loc

emptyStateData :: Int -> StateData
emptyStateData ticks = StateData 
  { store = (0 :: Int, IM.empty)
  , maxTicks = ticks
  , currTicks = ticks
  , threadQueue = []
  , finalAnswer = EmptyVal }

emptyStateData' :: StateData
emptyStateData' = emptyStateData 1

-- Thread control
type Thread = () -> InterpM ExpVal
type TId = Int -- Thread ID
type ThreadStorage = IM.IntMap Thread

type Mutex = (Bool, [TId])
type MId = Int -- Mutex ID
type MutexStorage = IM.IntMap Mutex

modifyStateData :: (StateData -> StateData) -> InterpM ()
modifyStateData f = do
  sdata <- get
  put $ f sdata

handleStateData :: (StateData -> a) -> InterpM a
handleStateData f = do
  sdata <- get
  return $ f sdata

newThread :: Thread -> InterpM TId
newThread th = do
  sdata <- get
  let tid = freeTId sdata
  updateThread tid th
  return tid

updateThread :: TId -> Thread -> InterpM ()
updateThread tid th = 
  modifyStateData (\sdata -> 
    sdata {threads = IM.insert tid th (threads sdata)})

freeTId :: StateData -> TId
freeTId sdata = freeId (threads sdata)

enqueueThread :: TId -> InterpM ()
enqueueThread tid = modifyStateData (\sdata -> 
  sdata {threadQueue = threadQueue sdata ++ [tid]})

enqueueCurrThread :: Thread -> InterpM ()
enqueueCurrThread th = do
  sdata <- get
  updateThread (currTId sdata) th
  enqueueThread (currTId sdata)

enqueueNewThread :: Thread -> InterpM ()
enqueueNewThread th = do
  tid <- newThread th
  enqueueThread tid

-- placeThreadOnQueue :: Thread -> InterpM ()
-- placeThreadOnQueue th = do
--   sdata <- get
--   put $ sdata {threadQueue = threadQueue sdata ++ [th]}

getThread :: TId -> InterpM Thread
getThread tid = do
  sdata <- get
  case IM.lookup tid (threads sdata) of
    Just th -> return th
    Nothing -> throwErr (ErrorData "thread not found")

runNextThread :: InterpM ExpVal
runNextThread = do
  sdata <- get
  case threadQueue sdata of
    [] -> return $ finalAnswer sdata
    (tid:tids) -> do
      put $ sdata {currTicks = maxTicks sdata, threadQueue = tids}
      th <- getThread tid
      th ()

decrementTicks :: InterpM ()
decrementTicks = modifyStateData (\sdata -> 
  sdata {currTicks = currTicks sdata - 1})

isTimeExpired :: InterpM Bool
isTimeExpired = handleStateData (\sdata ->
  currTicks sdata == 0)

setFinalAnswer :: ExpVal -> InterpM ()
setFinalAnswer val = modifyStateData (\sdata -> 
  sdata {finalAnswer = val})

endCurrThread :: InterpM ()
endCurrThread = modifyStateData (\sdata -> 
  sdata {threadQueue = tail $ threadQueue sdata})

freeMId :: StateData -> MId
freeMId sdata = freeId (mutexes sdata)

newMutex :: InterpM ExpVal
newMutex = do
  sdata <- get
  let mid = freeMId sdata
  put $ sdata {mutexes = IM.insert mid (True, []) (mutexes sdata)}
  return (MutexRef mid)

-- TODO: continue from here please
isMutexOpened :: MId -> InterpM Bool
isMutexOpened mid = handleMutex mid (\opened _ -> return opened)

isMutexQueueEmpty :: MId -> InterpM Bool
isMutexQueueEmpty loc = handleMutex loc (\_ tids -> return $ null tids)

openMutex :: MId -> Bool -> InterpM ()
openMutex mid open = handleMutex mid (\_ tids -> updateMutex mid (open, tids))

placeThreadOnMutexQueue :: MId -> Thread -> InterpM ()
placeThreadOnMutexQueue loc th =
  handleMutex loc (\b ths -> void $ setLocVal loc (MutexVal b (ths ++ [th])))

scheduleMutexThread :: Loc -> InterpM ()
scheduleMutexThread loc = handleMutex loc $ \open (th:ths) ->  
  setLocVal loc (MutexVal open ths) >> placeThreadOnQueue th

handleMutex :: Loc -> (Bool -> [TId] -> InterpM a) -> InterpM a
handleMutex loc f = do
  sdata <- get
  case IM.lookup loc (mutexes sdata) of
    Just (opened, tids) -> f opened tids
    Nothing -> throwErr (ErrorData "mutex value expected")

updateMutex :: MId -> Mutex -> InterpM ()
updateMutex mid mut = modifyStateData (\sdata ->
  sdata {mutexes = IM.insert mid mut (mutexes sdata)})

freeId :: IM.IntMap a -> Int
freeId m = 1 +
  foldr (\ x y -> if x > y then x else y) 1 (IM.keys m)

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
     StateT StateData (WriterT w (ExceptT e m)) a -> m (Either e (a, w))
unwrapState s = runExceptT $ runWriterT $ evalStateT s emptyStateData'

runk :: String -> IO ()
runk str = do  
  res <- unwrapState (evalk (fst $ head $ parse expr str) emptyEnv endMainThreadCont)
  -- liftIO $ print res
  case res of
    Left (ErrorData errr) -> print errr
    Right (e, h) -> do
      putStr "Final answer: "
      print e
      mapM_ print h
      return ()
  
run :: String -> IO ()
run str = undefined
  
-- run :: String -> IO ()
-- run str = do  
--   res <- unwrapState (eval (fst $ head $ parse expr str) emptyEnv)
--   case res of
--     Left (ErrorData err) -> putStrLn ("Error: " ++ err)
--     Right (expres, _) ->
--       print expres

runFile :: FilePath ->  (String -> IO ()) -> IO ()
runFile file rf = do
  contents <- fileContents file
  rf contents

runFileK :: FilePath -> IO ()
runFileK file = runFile file runk

exs :: [String]
exs = [
  "let x = 200 in let f = proc (z) -(z,x) in let x = 100 in let g = proc (z) -(z,x) in -((f 1), (g 1))",
  "let f = proc(x,y) -(x,*(y,-(x,2))) in (f 5 6)",
  "letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 22)",
  "let p = proc(x) set x = 4 in let a = 3 in {(p a);a}"
  ]

exl = last exs


