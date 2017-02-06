module Simplebool where

import Parser

data Type = 
    TpBool
  | TpArr Type Type
  deriving (Show)

data Term = 
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmAbs String Type Term
  | TmApp Term Term
  deriving (Show)
        