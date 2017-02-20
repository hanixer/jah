{-# LANGUAGE TypeSynonymInstances #-}
module TailForm where 

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
import Text.PrettyPrint.HughesPJ ((<+>), ($+$))

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
          | LetRecExp [(String, [String], Exp)] Exp
          | CallExp Exp [Exp]
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
          deriving (Show)
          
newtype CpsProgram = CpsProg TfExp
    deriving (Show)
    
data SimpleExp = 
    CpsNumExp Int
  | CpsVarExp String
  | CpsDiffExp SimpleExp SimpleExp
  | CpsIsZeroExp SimpleExp
  | CpsProcExp [String] TfExp
  | CpsSumExp [SimpleExp]
  deriving (Show)

data TfExp = 
    CpsSimpleExp SimpleExp
  | CpsLetExp String SimpleExp TfExp
  | CpsLetrecExp [(String, [String], TfExp)] TfExp
  | CpsIfExp SimpleExp TfExp TfExp
  | CpsCallExp SimpleExp [SimpleExp]
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
            ps <- some (do name <- identifier
                           args <- argsListExp
                           _ <- strTok "="
                           pbody <- expr
                           return (name, args, pbody))
            _ <- strTok "in"
            lbody <- expr
            return (LetRecExp ps lbody)

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

sumExp :: Parser Exp
sumExp = strTok "sum" >> strTok "(" >> 
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
data EnvEntry = SimpleEnvEntry String ExpVal
              | RecEnvEntry [(String, [String], Exp)] Env
              deriving (Show)
type Env = [EnvEntry]

emptyEnv :: Env
emptyEnv = []

extendEnv :: String -> ExpVal -> Env -> Env
extendEnv var val env = SimpleEnvEntry var val : env

extendEnvRec :: [(String, [String], Exp)] -> Env -> Env
extendEnvRec ps env =
  RecEnvEntry ps env : env

extendEnvMany :: [(String, ExpVal)] -> Env -> Env
extendEnvMany [] env = env
extendEnvMany ((var,val):rest) env = 
  extendEnvMany rest $ extendEnv var val env

applyEnv :: Env -> String -> Maybe ExpVal
applyEnv (SimpleEnvEntry var val:env) svar = 
  if var == svar 
    then Just val 
    else applyEnv env svar
applyEnv (RecEnvEntry ps innerEnv : restEnv) svar = 
  case L.find (\(var, _, _) -> var == svar) ps of
    Just (_, args, body) -> 
      Just (ProcVal args body (extendEnvRec ps innerEnv))
    Nothing -> applyEnv restEnv svar
applyEnv _ _ = Nothing

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
operandCont (ProcVal args body savedEnv) [] randVals _ cont randVal = do
  env' <- allocateArgs args (reverse (randVal:randVals)) savedEnv
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
evalk (LetRecExp ps body) env cont = 
  evalk body (extendEnvRec ps env) cont
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
  
progToTf :: Program -> CpsProgram
progToTf (Program e) = 
  CpsProg $ expsToTf [e] $ \ses -> CpsSimpleExp (head ses)

expToTf :: Exp -> SimpleExp -> TfExp
expToTf (NumExp n) cont = CpsCallExp cont [CpsNumExp n]
expToTf (VarExp v) cont = CpsCallExp cont [CpsVarExp v]
expToTf (ProcExp args body) cont = 
  CpsCallExp cont [procExpToSimple args body]
expToTf (SumExp es) cont = 
  expsToTf es (\ses -> CpsCallExp cont [CpsSumExp ses])
expToTf (CallExp rator rands) cont = 
  expsToTf (rator : rands) (\ses ->
    CpsCallExp (head ses) (tail ses ++ [cont]))
expToTf (LetRecExp ps body) cont = 
  CpsLetrecExp (map convertLetrec ps) (expToTf body cont)
expToTf (IfExp e1 e2 e3) cont = 
  expsToTf [e1] $ \ses -> 
    CpsIfExp (head ses) (expToTf e2 cont) (expToTf e3 cont)
expToTf _ _ = undefined

convertLetrec :: (String, [String], Exp) -> (String, [String], TfExp)
convertLetrec (name, args, body) = 
  (name, args ++ [fresh], expToTf body (CpsVarExp fresh))
  where fresh = freshVar "k" (freeVars body args)

expsToTf :: [Exp] -> ([SimpleExp] -> TfExp) -> TfExp
expsToTf es builder = case span isExpSimple (reverse es) of
  (ses, e : rest) -> 
    let fresh = freshVar "x" (foldl1 L.union $ map (`freeVars` []) es) in
    let tfRest = expsToTf ((reverse rest) ++ [VarExp fresh] ++ (reverse ses)) builder in
    expToTf e (CpsProcExp [fresh] tfRest)
  (ses, []) -> builder $ map expToSimpleExp ses

expToSimpleExp :: Exp -> SimpleExp
expToSimpleExp (NumExp n) = CpsNumExp n
expToSimpleExp (VarExp n) = CpsVarExp n
expToSimpleExp (DiffExp e1 e2) = 
  CpsDiffExp (expToSimpleExp e1) (expToSimpleExp e2)
expToSimpleExp (IsZeroExp e) = CpsIsZeroExp (expToSimpleExp e)
expToSimpleExp (ProcExp args body) = procExpToSimple args body
expToSimpleExp (SumExp es) = CpsSumExp  (map expToSimpleExp es)
expToSimpleExp e = error $ "expression " ++ show e ++ " is not simple"

isExpSimple :: Exp -> Bool
isExpSimple (NumExp _) = True
isExpSimple (VarExp _) = True
isExpSimple (DiffExp e1 e2) = isExpSimple e1 && isExpSimple e2
isExpSimple (IsZeroExp e) = isExpSimple e
isExpSimple (ProcExp _ _) = True
isExpSimple (SumExp es) = all isExpSimple es
isExpSimple _ = False

procExpToSimple :: [String] -> Exp -> SimpleExp
procExpToSimple args body = 
  CpsProcExp (args ++ [fresh]) (expToTf body (CpsVarExp fresh))
  where fresh = freshVar "k" (freeVars body args)

freshVar :: String -> [String] -> String
freshVar try bound
  | try `elem` bound = freshVar (try ++ "1") bound
  | otherwise = try

freeVars :: Exp -> [String] -> [String]
freeVars (ProcExp args body) bound = freeVars body (args `L.union` bound) 
freeVars (IsZeroExp e) bound = freeVars e bound
freeVars (IfExp e1 e2 e3) bound = 
  foldl L.union [] (map (`freeVars` bound) [e1,e2,e3])
freeVars (LetExp var e1 e2) bound = 
  freeVars e1 bound `L.union` freeVars e2 ([var] `L.union` bound)
freeVars (VarExp var) bound = if var `elem` bound then [] else [var]
freeVars (LetRecExp ps body) bound = 
  freeProcs `L.union` freeVars body (procNames `L.union` bound)
  where procNames = map (\(a,_,_) -> a) ps
        freeProcs = foldl1 L.union $ map (\(_, args, pbody) -> 
          freeVars pbody (args `L.union` procNames `L.union` bound)) ps
freeVars _ _ = []

-- Errors
data ErrorData = ErrorData String
  deriving (Show)

throwErr :: ErrorData -> InterpM a
throwErr edat = lift $ lift $ throwE edat

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

-- Pretty
ppSimpleExp :: SimpleExp -> PP.Doc
ppSimpleExp (CpsNumExp n) = PP.int n
ppSimpleExp (CpsVarExp v) = PP.text v
ppSimpleExp (CpsDiffExp e1 e2) = 
  PP.text "-" <> PP.parens (ppSimpleExp e1 <> PP.text ", " <> ppSimpleExp e2)
ppSimpleExp (CpsIsZeroExp e) = PP.text "zero?" <> PP.parens (ppSimpleExp e)
ppSimpleExp (CpsProcExp args body) = 
  PP.text "proc" <> PP.parens (PP.cat $ PP.punctuate PP.comma (map PP.text args)) $+$
  PP.nest 2 (ppTfExp body)
ppSimpleExp (CpsSumExp es) = PP.text "sum" <> 
  PP.parens (PP.cat $ PP.punctuate PP.comma (map ppSimpleExp es))

ppTfExp :: TfExp -> PP.Doc
ppTfExp (CpsSimpleExp e) = ppSimpleExp e
ppTfExp (CpsLetExp var rhs body) = 
  PP.text "let = " <> ppSimpleExp rhs <> PP.text " in " $+$ ppTfExp body
ppTfExp (CpsIfExp e1 e2 e3) = 
  PP.text "if " <> ppSimpleExp e1 $+$ 
    PP.nest 2 ((PP.text "then " <> ppTfExp e2) $+$ (PP.text "else " <> ppTfExp e3))
ppTfExp (CpsCallExp e1 es) = PP.parens (PP.sep (map ppSimpleExp (e1 : es)))

ppCpsProgram (CpsProg e) = ppTfExp e

transandpp :: String -> PP.Doc
transandpp str = ppCpsProgram $ progToTf $ strtoprog str
