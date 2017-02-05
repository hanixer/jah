module Parser where

import Control.Monad (void)
import Control.Applicative
import Data.Char

newtype Parser a = P(String -> [(a, String)])

item :: Parser Char
item = P (\inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x, xs)])

parse :: Parser a -> String -> [(a, String)]
parse (P f) = f


instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of 
                          [] -> []
                          [(v, out)] -> [(g v, out)]
                          _ -> [])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px = P (\inp -> case parse pg inp of
                             [] -> []
                             [(g, out)] -> parse (fmap g px) out
                             _ -> [])

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
      [] -> []
      [(v, out)] -> parse (f v) out
      _ -> [])

instance Alternative Parser where
  empty = P (const [])
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v, out)] -> [(v, out)]
                         _ -> [])

sat :: (Char -> Bool) -> Parser Char
sat prede = do x <- item
               if prede x
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
string (c:cs) = char c >> string cs >> return (c:cs)

space :: Parser ()
space = void $ many (sat isSpace)

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

ident = do x <- lower
           xs <- (many (lower <|> digit))
           return (x:xs)

identifier = token ident

num = do x <- digit
         xs <- (many digit)
         return (read (x:xs) :: Int)

signednum = do 
  do char '-'
     n <- num
     return (- n)
 <|>
  num

number = token signednum

strTok s = token (string s)

binaryOp :: Parser a -> String -> (a -> a -> a) -> Parser a
binaryOp p op combiner = 
  do strTok op
     strTok "("
     x <- p
     strTok ","
     y <- p
     strTok ")"
     return (combiner x y)

gluglu = do cs <- many (char 'c')
            return cs  

oneAndMore p combiner = do
  x <- p
  xs <- ((oneAndMore p combiner) <|> return [])
  return (combiner (x:xs))

concatp p1 p2 = do
  p1
  p2




ints :: Parser [Int]
ints = do
  _ <- char '['
  n <- number
  ns <- many $ do
    _ <- char ','
    x <- number
    return x
  return (n:ns)

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = do
  x <- p
  xs <- many $ do
    _ <- sep
    p
  return (x:xs)

ints2 :: Parser [Int]
ints2 = do
  _ <- char '['
  xs <- sepby1 number (char ',')
  _ <- char ']'
  return xs

expr = do
  xs <- sepby1 term (char '+') 
  return $ sum xs

term = do
  xs <- sepby1 factor (char '*')
  return $ product xs

factor = number <|> do
  _ <- char '('
  e <- expr
  _ <- char ')'
  return e