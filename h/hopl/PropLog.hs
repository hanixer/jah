module PropLog where

import Data.Maybe (fromMaybe)
import Parser
import Control.Applicative ((<|>))
import Data.List (lookup, sort, nub)
import Text.Printf
import Control.Monad (forM_)

data Formula a = 
    FmTrue
  | FmFalse
  | Atom a
  | Not (Formula a)
  | And (Formula a) (Formula a)
  | Or (Formula a) (Formula a)
  | Imp (Formula a) (Formula a)
  | Iff (Formula a) (Formula a)
  deriving (Show)

type FmString = Formula String

formulaPrs :: Parser (Formula String)
formulaPrs = iffPrs

primaryPrs :: Parser FmString
primaryPrs = 
  (strtok "~" >> formulaPrs >>= \fm -> return $ Not fm) <|>  
  (strtok "true" >> return FmTrue) <|>
  (strtok "false" >> return FmFalse) <|>
  (identifier >>= \s -> return $ Atom s)

andPrs :: Parser FmString
andPrs = chainl primaryPrs (strtok "/\\" <|> strtok "&&" >> return And)

orPrs :: Parser FmString
orPrs = chainl andPrs (strtok "\\/" <|> strtok "||" >> return Or)

impPrs :: Parser FmString
impPrs = chainPrs orPrs "==>" Imp

iffPrs :: Parser FmString
iffPrs = chainPrs impPrs "<=>" Iff

chainPrs :: Parser FmString -> String -> (FmString -> FmString -> FmString) -> Parser FmString
chainPrs p tok f = chainl p (strtok tok >> return f)

onAtoms :: (a -> Formula a) -> Formula a -> Formula a
onAtoms f (Atom a) = f a
onAtoms f (Not fm) = Not $ onAtoms f fm
onAtoms f (And fm1 fm2) = And (onAtoms f fm1) (onAtoms f fm2)
onAtoms f (Or fm1 fm2) = Or (onAtoms f fm1) (onAtoms f fm2)
onAtoms f (Imp fm1 fm2) = Imp (onAtoms f fm1) (onAtoms f fm2)
onAtoms f (Iff fm1 fm2) = Iff (onAtoms f fm1) (onAtoms f fm2)
onAtoms _ fm = fm

overAtoms :: (a -> b -> b) -> Formula a -> b -> b
overAtoms f (Atom a) b = f a b
overAtoms f (Not fm) b = overAtoms f fm b
overAtoms f (And fm1 fm2) b = overAtoms f fm1 (overAtoms f fm2 b)
overAtoms f (Or fm1 fm2) b = overAtoms f fm1 (overAtoms f fm2 b)
overAtoms f (Imp fm1 fm2) b = overAtoms f fm1 (overAtoms f fm2 b)
overAtoms f (Iff fm1 fm2) b = overAtoms f fm1 (overAtoms f fm2 b)
overAtoms _ _ b = b

atomUnion :: Ord a => Formula a -> [a]
atomUnion fm = (sort . nub) $ overAtoms (:) fm []

type Env = [(String, Bool)]

eval :: FmString -> Env -> Bool
eval FmTrue _ = True
eval FmFalse _ = False
eval (Not fm) env = not (eval fm env)
eval (Atom a) env = fromMaybe (error "variable not found") (lookup a env)
eval (And fm1 fm2) env = eval fm1 env && eval fm2 env
eval (Or fm1 fm2) env = eval fm1 env || eval fm2 env
eval (Imp fm1 fm2) env = not (eval fm1 env) || eval fm2 env
eval (Iff fm1 fm2) env = eval fm1 env == eval fm2 env

justParseFm :: String -> FmString
justParseFm s = fromMaybe (error "parser error") (parse formulaPrs s)

run :: String -> Bool
run str = case parse formulaPrs str of
  Just t -> eval t []   
  Nothing -> error "parser error"

generEnvs :: FmString -> [Env]
generEnvs fm = go alls
  where alls = atomUnion fm
        go [] = [[]]
        go (x:xs) = do
          b <- [True, False]
          xs' <- go xs
          return ((x,b):xs')

printTruthTable :: FmString -> IO ()
printTruthTable fm = 
  forM_ envs $ \e -> do
    print e
    print $ eval fm e
  where envs = generEnvs fm
        atoms = atomUnion fm

tautology :: FmString -> Bool
tautology fm = all (fm `eval`) envs
  where envs = generEnvs fm

unsatisfiable :: FmString -> Bool
unsatisfiable fm = tautology (Not fm)

substit :: [(String, FmString)] -> FmString -> FmString
substit dict = onAtoms $ \a -> fromMaybe (Atom a) (lookup a dict)