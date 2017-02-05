module Monparse where

import Control.Monad.Reader
import Control.Monad.State

type Parser a = ReaderT Pos (StateT Pstring Maybe)  a
type Pstring = (Pos, String)
type Pos = (Int, Int)

item :: Parser Char
item = do
    (p, (c:cs)) <- get
    put (p, cs)
    return c

main = putStrLn "Hello World"