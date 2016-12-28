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
(p <|> q) input = p input ++ q input

(<*>) :: Parser (b -> a) -> Parser b -> Parser a
(p <*> q) input = 
  [(pv qv, rest) | (pv, qinput) <- p input, (qv, rest) <- q qinput]

infixl 3 <$>
(<$>) :: (b -> a) -> Parser b -> Parser a
f <$> p = succeed f <*> p

type Result a = Either a String
parser :: Parser a -> Input -> Result a
parser p s = case p s of
  [] -> Right "Erroneous input"
  ((res, rest):rs) -> Left res

p = ((symbol 3) <|> (((+) <$> symbol 3) <*> (symbol 4)))

data Steps result = Ok (Steps result)
                  | Fail (Steps result)
                  | Stop result

getresult :: Steps result -> result
getresult (Ok l) = getresult l
getresult (Fail l) = getresult l
getresult (Stop v) = v
