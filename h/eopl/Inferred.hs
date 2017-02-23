{-# LANGUAGE TypeSynonymInstances #-}
module Inferred where 

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Writer
import Parser 
import System.IO
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.List (union)
import Control.Monad.State
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ ((<+>), ($+$), ($$))

type Name = String

-- AST             
newtype Program = Program Exp
                deriving (Show)
data Exp = NumExp Int
          | IsZeroExp Exp
          | IfExp Exp Exp Exp
          | VarExp String
          | LetExp String Exp Exp
          | ProcExp String Type Exp
          | LetRecExp (Type, String, String, Type, Exp) Exp
          | CallExp Exp Exp
          | CompoundExp [Exp]
          -- Arithmetic
          | DiffExp Exp Exp
          | MinusExp Exp
          | SumExp [Exp]
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
          deriving (Show, Eq)

data Type =
    IntType
  | BoolType
  | ProcType Type Type
  | NoType
  deriving (Show, Eq)

primTypeParser :: Parser Type
primTypeParser = 
  (strTok "?" >> return NoType) <|>
  (strTok "int" >> return IntType) <|> 
  (strTok "bool" >> return BoolType) <|>
  (strTok "(" >> typeParser >>= \t -> strTok ")" >> return t)

typeParser :: Parser Type
typeParser = chainr primTypeParser (strTok "->" >> return ProcType)
          
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
    _ <- (strTok "," <|> strTok "")
    varListExp)
  return (v : concat vs)

argsListExp :: Parser [String]
argsListExp = do 
  _ <- strTok "("
  vs <- varListExp <|> return []
  _ <- strTok ")"
  return vs            

argAndTypeParser :: Parser (String, Type)
argAndTypeParser = do
  arg <- identifier
  t <- (strTok ":" >> typeParser) <|> (strTok "" >> return NoType)
  return (arg, t)

procExp :: Parser Exp
procExp = do 
  _ <- strTok "proc"
  _ <- strTok "("
  (arg, t) <- argAndTypeParser
  _ <- strTok ")"
  e <- expr
  return (ProcExp arg t e)

letrec :: Parser Exp
letrec = do _ <- strTok "letrec"
            ps <- some (do  tret <- typeParser <|> (strTok "" >> return NoType)
                            name <- identifier
                            _ <- strTok "("
                            (arg, targ) <- argAndTypeParser
                            _ <- strTok ")"
                            _ <- strTok "="
                            pbody <- expr
                            return (tret, name, arg, targ, pbody))
            _ <- strTok "in"
            lbody <- expr
            return (LetRecExp (head ps) lbody)

callExp :: Parser Exp
callExp = do _ <- strTok "("
             e1 <- expr
             es <- many expr
             _ <- strTok ")"
             return (CallExp e1 (head es))

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
      (do _ <- (strTok "," <|> empty)
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

sumExp :: Parser Exp
sumExp = (strTok "sum" <|> strTok "+") >> strTok "(" >> 
  commaSeparExpList >>= \es -> strTok ")" >> 
  return (SumExp es)


expr :: Parser Exp
expr = numExp <|> letExp <|> ifExp <|> procExp <|> letrec <|> callExp <|>
  compoundExp
  <|> isZeroExp
  <|> minusExp
  <|> diffExp
  <|> binOp "cons" ConsExp
  <|> unaryOp "car" CarExp
  <|> unaryOp "cdr" CdrExp
  <|> unaryOp "empty?" IsEmptyExp
  <|> emptyExp
  <|> listExp
  <|> condExp
  <|> unaryOp "print" PrintExp
  <|> sumExp
  <|> varExp

program :: Parser Program
program = do e <- expr
             return (Program e)

-- Expession values
data ExpVal = IntVal Int
            | BoolVal Bool
            | ConsVal ExpVal ExpVal
            | EmptyVal
            | ProcVal String Exp Env
            | RefVal Loc
  deriving (Eq)

instance Show ExpVal where
  show (IntVal n) = "<" ++ show n ++ ">"
  show (BoolVal b) = "<" ++ show b ++ ">"
  show (RefVal loc) = "<loc:" ++ show loc ++ ">"
  show (ProcVal vs _ _) = "<proc:" ++ show vs ++ ">"
  show EmptyVal = "<empty>"
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
data EnvEntry = SimpleEnvEntry String ExpVal
              | RecEnvEntry (String, String, Exp) Env
  deriving (Show, Eq)
type Env = [EnvEntry]
type TEnv = [(String, Type)]

emptyEnv :: Env
emptyEnv = []

extendEnv :: String -> ExpVal -> Env -> Env
extendEnv var val env = SimpleEnvEntry var val : env

extendEnvRec :: String -> String -> Exp -> Env -> Env
extendEnvRec name arg pbody env =
  RecEnvEntry (name, arg, pbody) env : env

extendEnvMany :: [(String, ExpVal)] -> Env -> Env
extendEnvMany [] env = env
extendEnvMany ((var,val):rest) env = 
  extendEnvMany rest $ extendEnv var val env

applyEnv :: Env -> String -> Maybe ExpVal
applyEnv (SimpleEnvEntry var val:env) svar = 
  if var == svar 
    then Just val 
    else applyEnv env svar
applyEnv (RecEnvEntry p@(var, args, body) innerEnv : restEnv) svar = 
  if var == svar 
    then Just (ProcVal args body (extendEnvRec var args body innerEnv))
    else applyEnv restEnv svar
applyEnv _ _ = Nothing

emptyTEnv :: TEnv
emptyTEnv = []

extendTEnv :: String -> Type -> TEnv -> TEnv
extendTEnv var t tenv = (var, t) : tenv

applyTEnv :: TEnv -> String -> Maybe Type
applyTEnv tenv var = L.lookup var tenv


-- Evaluation
type InterpM a = StateT StateData (WriterT [HistItem] (ExceptT ErrorData IO)) a

-- Continuations
type Cont = (ExpVal -> InterpM ExpVal)

applyCont :: Cont -> ExpVal -> InterpM ExpVal
applyCont cont = cont

endCont :: Cont
endCont val = do
  liftIO $ putStr "End of computation: "
  liftIO $ print val
  return EmptyVal

zeroCont :: Cont -> Cont
zeroCont cont (IntVal n) = applyCont cont $ BoolVal $ n == 0
zeroCont _ _ = throwErr (ErrorData "number is expected")

letExpCont :: Name -> Exp -> Env -> Cont -> Cont
letExpCont var body env cont val = do
  env' <- allocateArgs [var] [val] env
  evalk body env' cont

ifTestCont :: Exp -> Exp -> Env -> Cont -> Cont
ifTestCont thenExp elseExp env cont (BoolVal b) =
  if b
    then evalk thenExp env cont
    else evalk elseExp env cont
ifTestCont _ _ _ _ _ = throwErr (ErrorData "bool is expected")

operatorCont :: [Exp] -> Env -> Cont -> Cont 
operatorCont [] _ cont (ProcVal _ body env) =
  evalk body env cont
operatorCont (e:es) env cont ratorVal =
  evalk e env $ operandCont ratorVal es [] env cont
operatorCont _ _ _ _ = throwErr (ErrorData "operatorCont error")

operandCont :: ExpVal -> [Exp] -> [ExpVal] -> Env -> Cont -> Cont
operandCont (ProcVal args body savedEnv) [] randVals _ cont randVal = do
  env' <- allocateArgs [args] (reverse (randVal:randVals)) savedEnv
  evalk body env' cont
operandCont procVal (e:es) randVals env cont val = 
  evalk e env $ operandCont procVal es (val:randVals) env cont
operandCont _ _ _ _ _ _ = throwErr (ErrorData "operandCont error")

diffCont1 :: Exp -> Env -> Cont -> Cont
diffCont1 e2 env cont val1 = 
  evalk e2 env (diffCont2 val1 cont)

diffCont2 :: ExpVal -> Cont -> Cont
diffCont2 (IntVal n1) cont (IntVal n2) = 
  applyCont cont (IntVal (n1 - n2))
diffCont2 _ _ _  = throwErr (ErrorData "number is expected")

compoundCont :: [Exp] -> Env -> Cont -> Cont
compoundCont [] _ cont val = cont val
compoundCont (e:es) env cont _ = 
  evalk e env $ compoundCont es env cont

printCont :: Cont -> Cont
printCont cont val = do
  liftIO $ putStr "Program output: "
  liftIO $ print val
  applyCont cont EmptyVal

evalk :: Exp -> Env -> Cont -> InterpM ExpVal
evalk (VarExp var) env cont = do
  val <- resolveVar var env
  applyCont cont val
evalk (NumExp n) _ cont = 
  applyCont cont $ IntVal n
evalk (ProcExp arg _ body) env cont =
  applyCont cont $ ProcVal arg body env
evalk (IsZeroExp e) env cont = 
  evalk e env $ zeroCont cont
evalk (IfExp e1 e2 e3) env cont =
  evalk e1 env $ ifTestCont e2 e3 env cont
evalk (LetExp name rhs body) env cont =
  evalk rhs env $ letExpCont name body env cont
evalk (DiffExp e1 e2) env cont =
  evalk e1 env (diffCont1 e2 env cont) 
evalk (CallExp e1 e2) env cont =
  evalk e1 env $ operatorCont [e2] env cont
evalk (CompoundExp (e:es)) env cont =
  evalk e env $ compoundCont es env cont
evalk (LetRecExp (_, name, arg, _, pbody) body) env cont = 
  evalk body (extendEnvRec name arg pbody env) cont
evalk (PrintExp e) env cont = 
  evalk e env $ printCont cont
evalk e _ _ = throwErr $ ErrorData $ "Evalk error " ++ show e

resolveVar :: Name -> Env -> InterpM ExpVal
resolveVar var env = 
  case applyEnv env var of
    Just v -> return v
    Nothing -> throwErr (ErrorData $ "Variable " ++ var ++ " cannot be resolved")

allocateArgs :: [Name] -> [ExpVal] -> Env -> InterpM Env
allocateArgs vars vals env = 
  return $ extendEnvMany (zip vars vals) env

typeof :: Exp -> TEnv -> Maybe Type
typeof (NumExp _) _ = return IntType
typeof (VarExp v) tenv = applyTEnv tenv v
typeof (IsZeroExp e) tenv = case typeof e tenv of
  Just IntType -> return BoolType
  _ -> empty
typeof (IfExp e1 e2 e3) tenv = 
  case (typeof e1 tenv,
        typeof e2 tenv,
        typeof e3 tenv) of
    (Just BoolType, Just t1, Just t2) -> if t1 == t2
      then return t1
      else empty
    _ -> empty
typeof (ProcExp arg t body) tenv = do
  t2 <- typeof body (extendTEnv arg t tenv)
  return (ProcType t t2)
typeof (LetRecExp (tres, name, arg, targ, pbody) body) tenv = do
  let tp = ProcType targ tres
  tpbody <- typeof pbody (extendTEnv name tp (extendTEnv arg targ tenv))
  if tpbody == tres
    then typeof body (extendTEnv name tp tenv)
    else empty
typeof (CallExp e1 e2) tenv = do
  t1 <- typeof e1 tenv
  t2 <- typeof e2 tenv
  case t1 of
    ProcType tin tout -> if tin == t2
      then return tout
      else empty
    _ -> empty
typeof (DiffExp e1 e2) tenv = do
  t1 <- typeof e1 tenv
  t2 <- typeof e2 tenv
  case (t1, t2) of
    (IntType, IntType) -> return IntType
    _ -> empty
typeof (LetExp var rhs body) tenv = do
  t1 <- typeof rhs tenv
  typeof body (extendTEnv var t1 tenv)
typeof _ _ = empty

-- Errors
data ErrorData = ErrorData String
  deriving (Show)

throwErr :: ErrorData -> InterpM a
throwErr edat = lift $ lift $ throwE edat

te :: String -> InterpM a
te s = throwErr (ErrorData s)

-- Storage
data HistItem = 
    NewRefItem Loc
  | SetRefItem Loc ExpVal
  | DerefItem Loc ExpVal

instance Show HistItem where
  show (NewRefItem n) = "[" ++ show n ++ "] new" 
  show (SetRefItem n val) = "[" ++ show n ++ "] <- " ++ show val
  show (DerefItem n val) = "[" ++ show n ++ "] => " ++ show val

type Hist = [HistItem]
type Store = (Int, IM.IntMap ExpVal)
type Loc = Int
type Thread = () -> InterpM ExpVal
type StateData = Int

-- Test helpers
evalFile :: FilePath -> IO ()
evalFile file = do
  handle <- openFile file ReadMode
  _ <- hGetContents handle
  return ()

fileContents :: FilePath -> IO String
fileContents file = do 
  handle <- openFile file ReadMode
  hGetContents handle

parseFile :: FilePath -> IO ()
parseFile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  print contents
  print $ fst $ head $ parse expr contents

unwrapState
  :: (Monoid w, Monad m) =>
     StateT StateData (WriterT w (ExceptT e m)) a -> m (Either e (a, w))
unwrapState s = runExceptT $ runWriterT $ evalStateT s 0

runk :: String -> IO ()
runk str = do  
  res <- unwrapState (evalk (fst $ head $ parse expr str) emptyEnv endCont)
  case res of
    Left (ErrorData errr) -> print errr
    Right (e, h) -> do
      putStr "Final answer: "
      print e
      mapM_ print h
      return ()
  
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

exl :: String
exl = last exs

strtoexpr :: String -> Exp
strtoexpr str = fst $ head $ parse expr str

strtoprog :: String -> Program
strtoprog str = fst $ head $ parse program str

-- Tests
runtests :: IO ()
runtests = forM_ tests run
  where run (str, val) = case parse program str of
          [(Program e, _)] -> run1 e str val
          _ -> putStrLn $ "Parse failed: " ++ str
        run1 e str val = do
          res <- unwrapState (evalk e env return)
          case res of
            Left (ErrorData _) -> 
              unless (val == EmptyVal) $ report str EmptyVal EmptyVal
            Right (resval, _) -> 
              unless (resval == val) $ report str val resval
        env = extendEnv "x" (IntVal 10) emptyEnv
        report str expect observ =
          putStrLn ("For: " ++ str) >>
          putStrLn ("Expected: " ++ show expect) >> 
          putStrLn ("Got: " ++ show observ ++ "\n")

runtytests :: IO ()
runtytests = forM_ tytests run
  where run (str, t) = case parse program str of
          [(Program e, _)] -> run1 e str t
          _ -> putStrLn $ "Parse failed: " ++ str
        run1 e str t = let tres = typeof e tenv in
          unless (tres == t) $ report str t tres
        tenv = extendTEnv "x" IntType emptyTEnv
        report str expect observ =
          putStrLn ("For: " ++ str) >>
          putStrLn ("Expected: " ++ show expect) >> 
          putStrLn ("Got: " ++ show observ ++ "\n")

tests :: [(String, ExpVal)]
tests = 
  [
  ("11",IntVal 11),
  ("-33",IntVal (-33)),
  ("-(44,33)",IntVal 11),
  ("-(-(44,33),22)",IntVal (-11)),
  ("-(55, -(22,11))",IntVal 44),
  ("x",IntVal 10),
  ("-(x,1)",IntVal 9),
  ("-(1,x)",IntVal (-9)),
  ("foo",EmptyVal),
  ("-(x,foo)",EmptyVal),
  ("if zero?(0) then 3 else 4",IntVal 3),
  ("if zero?(1) then 3 else 4",IntVal 4),
  ("-(zero?(0),1)",EmptyVal),
  ("-(1,zero?(0))",EmptyVal),
  ("if 1 then 2 else 3",EmptyVal),
  ("if zero?(-(11,11)) then 3 else 4",IntVal 3),
  ("if zero?(-(11, 12)) then 3 else 4",IntVal 4),
  ("if zero?(-(11, 11)) then 3 else foo",IntVal 3),
  ("if zero?(-(11,12)) then foo else 4",IntVal 4),
  ("let x = 3 in x",IntVal 3),
  ("let x = 3 in -(x,1)",IntVal 2),
  ("let x = -(4,1) in -(x,1)",IntVal 2),
  ("let x = 3 in let y = 4 in -(x,y)",IntVal (-1)),
  ("let x = 3 in let x = 4 in x",IntVal 4),
  ("let x = 3 in let x = -(x,1) in x",IntVal 2),
  ("(proc(x) -(x,1)  30)",IntVal 29),
  ("let f = proc (x) -(x,1) in (f 30)",IntVal 29),
  ("(proc(f)(f 30)  proc(x)-(x,1))",IntVal 29),
  ("((proc (x) proc (y) -(x,y)  5) 6)",IntVal (-1)),
  ("(proc (x y) -(x,y)  5 6)",IntVal (-1)),
  ("let f = proc(x y) -(x,y) in (f -(10,5) 6)",IntVal (-1)),
  ("let fix =  proc (f)            let d = proc (x) proc (z) ((f (x x)) z)            in proc (n) ((f (d d)) n)in let    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)in let times4 = (fix t4m)   in (times4 3)",IntVal 12),
  ("        (proc (twice)           ((twice proc (z) -(z,1)) 11)         proc (f) proc (x) (f (f x)))",IntVal 9),
  ("      let twice = proc(f x k)                    (f x  proc (z) (f z k))      in (twice           proc (x k) (k -(x,1))          11          proc(z) z)",IntVal 9),
  ("       let f = proc (x) -(x,1)       in (f 27)",IntVal 26),
  ("       let f = proc (x) -(x,1)       in (f (f 27))",IntVal 25),
  ("       let f = proc (x) proc (y) -(x,y)       in ((f 27) 4)",IntVal 23),
  ("      let f = proc (x) proc (y) -(x, y)      in let g = proc (z) -(z, 1)      in ((f 27) (g 11))",IntVal 17),
  ("      let f = proc (x) -(x, 1)      in if zero?((f 1)) then 11 else 22",IntVal 11),
  ("letrec f(x) = 17 in 34",IntVal 34),
  ("letrec f(x y) = -(x,y) in -(34, 2)",IntVal 32),
  ("       letrec even(x) = if zero?(x) then zero?(0) else (odd -(x,1))              odd (x) = if zero?(x) then zero?(1) else (even -(x,1))       in (even 5)", BoolVal False),
  ("      letrec fib(n) = if zero?(n) then 1                      else if zero?(-(n,1)) then 1                      else -((fib -(n,1)), -(0, (fib -(n,2))))      in (fib 5)",IntVal 8)
  ]

tytests :: [(String, Maybe Type)]
tytests = 
  [
  ("11",Just IntType),
  ("-33",Just IntType),
  ("-(44,33)",Just IntType),
  ("-(-(44,33),22)",Just IntType),
  ("-(55, -(22,11))",Just IntType),
  ("x",Just IntType),
  ("-(x,1)",Just IntType),
  ("-(1,x)",Just IntType),
  ("zero?(-(3,2))",Just BoolType),
  ("-(2,zero?(0))",Nothing),
  ("foo",Nothing),
  ("-(x,foo)",Nothing),
  ("if zero?(1) then 3 else 4",Just IntType),
  ("if zero?(0) then 3 else 4",Just IntType),
  ("if zero?(-(11,12)) then 3 else 4",Just IntType),
  ("if zero?(-(11, 11)) then 3 else 4",Just IntType),
  ("if zero?(1) then -(22,1) else -(22,2)",Just IntType),
  ("if zero?(0) then -(22,1) else -(22,2)",Just IntType),
  ("if zero?(0) then 1 else zero?(1)",Nothing),
  ("if 1 then 11 else 12",Nothing),
  ("let x = 3 in x",Just IntType),
  ("let x = 3 in -(x,1)",Just IntType),
  ("let x = -(4,1) in -(x,1)",Just IntType),
  ("let x = 3 in let y = 4 in -(x,y)",Just IntType),
  ("let x = 3 in let x = 4 in x",Just IntType),
  ("let x = 3 in let x = -(x,1) in x",Just IntType),
  ("(proc(x : int) -(x,1)  30)",Just IntType),
  ("(proc(x : (int -> int)) -(x,1)  30)",Nothing),
  ("let f = proc (x : int) -(x,1) in (f 30)",Just IntType),
  ("(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))",Just IntType),
  ("((proc (x : int) proc (y : int) -(x,y)  5) 6)",Just IntType),
  ("let f = proc (x : int) proc (y : int) -(x,y) in ((f -(10,5)) 3)",Just IntType),
  ("letrec int f(x : int) = -(x,1) in (f 33)",Just IntType),
  ("letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), -2) in (f 4)",Just IntType),
  ("let m = -5  in letrec int f(x : int) = if zero?(x) then -((f -(x,1)), m) else 0 in (f 4)",Just IntType),
  ("letrec int double (n : int) = if zero?(n) then 0                                   else -( (double -(n,1)), -2)in (double 3)",Just IntType),
  ("proc (x : int) -(x,1)", Just (ProcType IntType IntType)),
  ("proc (x : int) zero?(-(x,1))", Just (ProcType IntType BoolType)),
  ("let f = proc (x : int) -(x,1) in (f 4)",Just IntType),
  ("let f = proc (x : int) -(x,1) in f",
  Just (ProcType IntType IntType)),
  ("proc(f : (int -> bool)) (f 3)",
  Just (ProcType (ProcType IntType BoolType) BoolType)),
  ("proc(f : (bool -> bool)) (f 3)",Nothing),
  ("proc (x : int) proc (f : (int -> bool)) (f x)",
  Just
    (ProcType IntType (ProcType (ProcType IntType BoolType) BoolType))),
  ("proc (x : int) proc (f : (int -> (int -> bool))) (f x)",
  Just
    (ProcType IntType
        (ProcType (ProcType IntType (ProcType IntType BoolType))
          (ProcType IntType BoolType)))),
  ("proc (x : int) proc (f : (int -> (int -> bool))) (f zero?(x))",Nothing),
  ("((proc(x : int) proc (y : int)-(x,y)  4) 3)",Just IntType),
  ("(proc (x : int) -(x,1) 4)",Just IntType),
  ("letrec int f(x : int) = -(x,1)in (f 40)",Just IntType),
  ("(proc (x : int)      letrec bool loop(x : bool) =(loop x)       in x     1)",Just IntType),
  ("let times = proc (x : int) proc (y : int) -(x,y)  in letrec int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1)))   in fact",
  Just (ProcType IntType IntType)),
  ("let times = proc (x : int) proc (y : int) -(x,y)  in letrec int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1)))   in (fact 4)",Just IntType)  
  ]