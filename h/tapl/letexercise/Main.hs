module Main where

import Parser
import Control.Applicative ((<|>), empty)
import Data.List (elemIndex)

data Type = 
    TpBool
  | TpArr Type Type
  deriving (Show, Eq)

data Term = 
    TmTrue
  | TmFalse
  | TmVar String
  | TmVarNameless Int
  | TmIf Term Term Term
  | TmAbs String Type Term
  | TmApp Term Term
  | TmLet String Term Term
  deriving (Show)

-- Parsing

variab :: Parser String
variab = do
  v <- identifier
  if v `elem` ["true", "false", "lambda", "let", "in", "if", "then", "else", "Bool"]
    then empty
    else return v
        
termPr :: Parser Term
termPr = chainl atomPr (strtok "" >> return TmApp)

atomPr :: Parser Term
atomPr = 
  (strtok "true" >> return TmTrue) <|>
  (strtok "false" >> return TmFalse) <|>
  (strtok "if" >> termPr >>= \t1 -> strtok "then" >> termPr >>= \t2 -> strtok "else" >> termPr >>= \t3 -> return $ TmIf t1 t2 t3) <|>
  (strtok "lambda" >> variab >>= \x -> strtok ":" >>  typePr >>= \tT -> strtok "." >> termPr >>= \t2 -> return $ TmAbs x tT t2) <|>
  (strtok "let" >> variab >>= \x -> strtok "=" >> termPr >>= \t1 -> strtok "in" >> termPr >>= \t2 -> return $ TmLet x t1 t2) <|>
  (strtok "(" >> termPr >>= \t -> strtok ")" >> return t) <|>
  (variab >>= \x -> return $ TmVar x)

typePr :: Parser Type
typePr = chainr atomTypePr (strtok "->" >> return TpArr)

atomTypePr :: Parser Type
atomTypePr =
  (strtok "Bool" >> return TpBool) <|>
  (strtok "(" >> typePr >>= \tT -> strtok ")" >> return tT)

-- Evaluation
isVal :: Term -> Bool
isVal TmAbs{} = True
isVal TmTrue = True
isVal TmFalse = True
isVal _ = False

toNameless :: Term -> [String] -> Term
toNameless (TmVar v) env = case elemIndex v env of
  Nothing -> error ("variable " ++ v ++ " not found")
  (Just i) -> TmVarNameless i
toNameless (TmIf t1 t2 t3) env = TmIf (toNameless t1 env) (toNameless t2 env) (toNameless t3 env)
toNameless (TmAbs x tyT t) env = TmAbs x tyT (toNameless t (x:env))
toNameless (TmApp t1 t2) env = TmApp (toNameless t1 env) (toNameless t2 env)
toNameless (TmLet x rhs body) env = TmLet x (toNameless rhs env) (toNameless body (x:env))
toNameless t _ = t

eval :: Term -> Term
eval (TmIf TmTrue t2 _)  = t2
eval (TmIf TmFalse _ t3)  = t3
eval (TmIf t1 t2 t3)  = TmIf (eval t1 ) t2 t3
eval (TmApp (TmAbs x tyT t11) t2)  = subst t11 0 t2
eval (TmApp t1 t2)  
  | not (isVal t1) = TmApp (eval t1 ) t2
  | not (isVal t2) = TmApp t1 (eval t2 )
  | otherwise = error "Cannot evaluate application"
eval (TmLet x rhs body)
  | isVal rhs = subst body 0 rhs
  | otherwise = TmLet x (eval rhs) body
eval t  
  | isVal t = t
  | otherwise = error $ "Cannot evaluate term: " ++ show t

evalAll :: Term -> Term
evalAll t
  | isVal t = t
  | otherwise = evalAll (eval t)

subst :: Term -> Int -> Term -> Term
subst t@(TmVarNameless y) j s
  | j == y = s
  | otherwise = t
subst (TmAbs x tyT t1) j s = TmAbs x tyT $ subst t1 (j + 1) (shift s 1 0)
subst (TmApp t1 t2) j s = TmApp (subst t1 j s) (subst t2 j s)
subst (TmIf t1 t2 t3) j s = TmIf (subst t1 j s) (subst t2 j s) (subst t3 j s)
subst t _ _ = t

shift :: Term -> Int -> Int -> Term
shift (TmVarNameless k) d c
  | k < c = TmVarNameless k
  | otherwise = TmVarNameless (k + d)
shift (TmAbs x tyT t) d c = TmAbs x tyT $ shift t d (c + 1)
shift (TmApp t1 t2) d c = TmApp (shift t1 d c) (shift t2 d c)
shift (TmIf t1 t2 t3) d c = TmIf (shift t1 d c) (shift t2 d c) (shift t3 d c)
shift t _ _ = t

evalString s =
  let t = toNameless (parseJust termPr s) [] in 
  go t
  where go t
          | isVal t = t
          | otherwise = go (eval t)

-- Type checking
typeOf :: Term -> [Type] -> Type
typeOf TmTrue _ = TpBool
typeOf TmFalse _ = TpBool
typeOf (TmVarNameless k) env = env !! k
typeOf (TmIf t1 t2 t3) env =
  if t1T == TpBool
    then if t2T == t3T 
            then t2T
            else error "Branches of 'if' must have the same type"
    else error "Condition of 'if' must have Bool type"
  where t1T = typeOf t1 env
        t2T = typeOf t2 env
        t3T = typeOf t3 env
typeOf (TmAbs x tyT t1) env =
  TpArr tyT $ typeOf t1 (tyT:env)
typeOf (TmApp t1 t2) env = case t1T of
  TpArr t2T tT -> tT
  _ -> error "wrong type in application"
  where t1T = typeOf t1 env
        t2T = typeOf t2 env
typeOf (TmLet x rhs body) env =
  bodyT
  where rhsT = typeOf rhs env
        bodyT = typeOf body (rhsT:env)
typeOf t env = error $ "Undefined type: " ++ show t

typeOf' :: Term -> Type
typeOf' t = typeOf t []

-- Use
run :: String -> IO ()
run str = case parse termPr str of
  Nothing -> print "Parse error"
  Just t -> do
    let nlt = toNameless t []
    let tT = typeOf' nlt
    let t' = evalAll nlt
    putStrLn $ show t' ++ " : " ++ show tT

main = undefined