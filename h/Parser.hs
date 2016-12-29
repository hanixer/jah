module Parser where

import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = P(String -> [(a, String)])

item :: Parser Char
item = P (\inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x, xs)])

parse :: Parser a -> String -> [(a, String)]
parse (P f) = \inp -> f inp


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

strTok s = token (string s)
