{-# LANGUAGE TypeSynonymInstances #-}
module Parser2 where

import Control.Monad.State
import Control.Applicative

type Parser a = StateT String Maybe a

parse :: Parser a -> String -> Maybe a
parse = evalStateT

item :: Parser Char
item = do
  s <- get
  case s of 
    [] -> empty
    (c:cs) -> put cs >> return c

sat :: (Char -> Bool) -> Parser Char
sat f = do
  c <- item
  if f c
    then return c
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
string (c:cs) = char c >> string cs >> return (c:cs)

--------------------------------------------------
data Tr a = Tr [Tr a]
          | Lf a
  deriving (Show)

-- expr :: Parser (Tr String)
-- expr = do
--   t <- term
--   ts <- many $ do
--     _ <- char '+'
--     term

--   return $ foldl (\x acc -> Tr [Lf "+", x, acc]) (Tr []) (t:ts)

expr2 :: Parser (Tr String)
expr2 = chainl term2 (char '+' >> return (\x y -> Tr [Lf "+", x, y]))  

-- term :: Parser (Tr String)
-- term = do
--   f <- factor
--   fs <- many $ do
--     _ <- char '^'
--     factor
--   return $ foldr (\x acc -> Tr [Lf "^", Lf x, acc]) (Tr []) (show (length fs):fs)

term2 :: Parser (Tr String)
term2 = chainr factor (char '^' >> return (\x y -> Tr [Lf "^", x, y]))  

factor :: Parser (Tr String)
factor = some digit >>= \s -> return $ Lf s

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = do
  x <- p
  rest x
  where rest x = (do f <- op
                     x' <- p
                     rest $ f x x') 
                <|> return x

chainr :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr p op = do
  x <- p
  do
    f <- op
    y <- chainr p op
    return $ f x y
   <|> return x





main = putStrLn "Hello World"