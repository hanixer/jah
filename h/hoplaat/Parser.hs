module Parser where

import Control.Monad.Except
import Control.Applicative
import Data.Char (isAlpha, isSpace)

newtype Parser a = P (String -> Except (String, String) (a, String))

parse :: Parser a -> String -> Either (String, String) (a, String)
parse (P p) inp = runExcept $ p inp

instance Functor Parser where
  fmap g p = P $ \inp ->
    case parse p inp of
      Left (err, rest) -> throwError (err, rest)
      Right (a, rest) -> return (g a, rest)

instance Applicative Parser where
  pure a = P $ \inp ->
    return (a, inp)
  pg <*> pa = P $ \inp -> case parse pg inp of
    Left e -> throwError e
    Right (g, rest1) -> case parse pa rest1 of
      Left e -> throwError e
      Right (a, rest2) -> return (g a, rest2)

instance Monad Parser where
  p >>= g = P $ \inp -> case parse p inp of
    Left e -> throwError e
    Right (a, rest1) -> case parse (g a) rest1 of
      Left e -> throwError e
      Right res -> return res

instance Alternative Parser where
  empty = P $ \inp -> throwError ("No alternative", inp)
  p <|> q = P $ \inp -> case parse p inp of
    Left _ ->  case parse q inp of
      Left e -> throwError e
      Right res -> return res
    Right res -> return res

item :: Parser Char
item = P $ \inp -> case inp of
  "" -> throwError ("End of string", "")
  (c:cs) -> return (c, cs)

failp :: String -> Parser a
failp s = P $ \inp -> throwError (s, inp)

char :: Char -> Parser Char
char c = item >>= \c2 -> if c == c2 then return c else failp "Wrong character"

str :: String -> Parser String
str "" = P $ \inp -> return ("", inp)
str (c:cs) = char c >>= \_ -> str cs >>= \_ -> return (c:cs)

sat :: (Char -> Bool) -> Parser Char
sat f = item >>= \c -> if f c then return c else failp "Predicate failed"

alpha :: Parser Char
alpha = sat isAlpha

wspace :: Parser Char
wspace = sat isSpace

ident :: Parser String
ident = some alpha

token :: Parser a -> Parser a
token p = do
  _ <- many wspace
  r <- p
  _ <- many wspace
  return r

identifier :: Parser String
identifier = token ident

string :: String -> Parser String
string s = token $ str s

