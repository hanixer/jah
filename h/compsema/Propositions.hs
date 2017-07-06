module Propositions where

import Data.List (nub, sort, lookup)
import Data.Maybe (fromJust)

data Form = Var String
          | And [Form]
          | Or [Form]
          | Not Form
  deriving (Show)

type Bindings = [(String, Bool)]

allNames :: Form -> [String]
allNames (Var var) = [var]
allNames (Not p) = allNames p
allNames (And ps) = (sort . nub . concat) $ map allNames ps
allNames (Or ps) = allNames (And ps)

genBindings :: [String] -> [Bindings]
genBindings [] = [[]]
genBindings (s:ss) = do
  b <- [True, False]
  other <- others
  return $ (s, b):other
  where others = genBindings ss

allBindings :: Form -> [Bindings]
allBindings = genBindings . allNames

eval :: Bindings -> Form -> Bool
eval bs (Var var) = fromJust $ lookup var bs
eval bs (Not p) = not $ eval bs p
eval bs (And ps) = and $ map (bs `eval`) ps
eval bs (Or ps) = or $ map (bs `eval`) ps

tautology :: Form -> Bool
tautology p = all (`eval` p) (allBindings p)

satisfiable :: Form -> Bool
satisfiable p = any (`eval` p) (allBindings p)

contradiction :: Form -> Bool
contradiction = not . satisfiable

implies :: Form -> Form -> Bool
implies form1 form2 = contradiction $ And [form1, Not form2]

bs :: Bindings
bs = [("p", True), ("q", False)]