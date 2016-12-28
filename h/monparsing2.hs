module MyModule where
--import Prelude hiding ((>>=), return, Monad)

import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = P(String -> [(a, String)])

--zero :: P a
--zero = P(\inp -> [])

item :: Parser Char
item = P (\inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x, xs)])

parse (P f) = \inp -> f inp


{-
sat :: (Char -> Bool) -> P Char
sat p = item `bind` \x1 -> 
  if (p x1) then (result x1) else zero  

char :: Char -> P Char
char x = sat (\y -> x == y)

digit :: P Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: P Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: P Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

plus :: P a -> P a -> P a
p `plus` q = \inp -> (p inp ++ q inp)

letter :: P Char
letter = lower `plus` upper

alphanum :: P Char
alphanum = letter `plus` digit

threechar = letter `bind` \c1 ->
                            letter `bind` \c2 ->
                                            letter `bind` \c3 -> result [c3, c2, c1]
word :: P String
word = (letter `bind` \c -> 
           word `bind` \xs -> result (c:xs)) 
       `plus` (result "")

union :: P a -> P a -> P a
p `union` q = \inp -> case p inp of
  [] -> q inp
  [(v, inp)] -> [(v, inp)]


word2 = neWord `union` result ""
  where
    neWord = letter `bind` \x  ->
             word2   `bind` \xs -> result (x:xs)
-}
{-
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
-}

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of 
                          [] -> []
                          [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px = P (\inp -> case parse pg inp of
                             [] -> []
                             [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  p >>= f = P (\inp -> case (parse p inp) of
                       [] -> []
                       [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\inp -> [])
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v, out)] -> [(v, out)])

--(+++) :: Parser a -> Parser a -> Parser a
--p +++ q = P (\inp -> case parse (p ++ q)

sat :: (Char -> Bool) -> Parser Char
sat pred = do x <- item
              if pred x
                then return x
                else empty

char :: Char -> Parser Char
char x = sat (\y -> x == y)


digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

--alphanum :: Parser Char
--alphanum = letter `plus` digit

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}



space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

ident = do x <- lower
           xs <- (many lower)
           return (x:xs)

identifier = token ident

num = do x <- digit
         xs <- (many digit)
         return (read (x:xs) :: Int)

number = token num

int :: Parser Int
int = do char '-'
         n <- num
         return (-n)
           <|> num

int2 = (char '-' >>= \_ ->
           num >>= \n ->
           return (-n))
  <|> num
  

-- Expressions
expr :: Parser Int
expr = do t <- term
          do char '+'
             e <- expr
             return (t + e)
           <|> return t
  
term :: Parser Int
term = do f <- factor
          do char '*'
             e <- expr
             return (f + e)
           <|> return f

factor :: Parser Int
factor = do char '('
            e <- expr
            char ')'
            return e
  <|> number
             
