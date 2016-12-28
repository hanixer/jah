module Moduleing where
import Prelude hiding ((<*>), (<$>))


infixl 3 <|>
infixl 4 <*>

type Symbol = Int
type Input = [Symbol]
type Parser a = Input -> [(a, Input)]

succeed :: a -> Parser a
succeed v input = [(v, input)]

symbol :: Symbol -> Parser Symbol
symbol a (b:bs) = if a == b then [(b, bs)] else []
symbol a [] = []

(<|>) :: Parser a -> Parser a -> Parser a
(p <|> q) input = case p input of
  [] -> q input
  result -> result

(<*>) :: Parser (b -> a) -> Parser b -> Parser a
(p <*> q) input = case  p input of
  [] -> []
  [(f, qinput)] -> case q qinput of
    [] -> []
    [(v, qoutput)] -> [(f v, qoutput)]

infixl 3 <$>
(<$>) :: (b -> a) -> Parser b -> Parser a
f <$> p = succeed f <*> p

type Result a = Either a String
parser :: Parser a -> Input -> Result a
parser p s = case p s of
  [] -> Right "Erroneous input"
  ((res, rest):rs) -> Left res
