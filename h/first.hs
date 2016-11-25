type Parser a = String -> [(a, String)]

success :: a -> Parser a
success v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \inp -> case parse p inp of
  [] -> []
  [(v, out)] -> parse (f v) out

(+++) :: Parser a ->  Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
  [] -> parse q inp
  [(v, out)] -> [(v, out)]


ppp = item >>>= \v -> item >>>= \v2 -> item >>>= \v3 -> success [v, v3]

a = item >>>= \v -> if v == 'a' then success True else failure
b = item >>>= \v -> if v == 'b' then success True else failure
aorb = a +++ b

sat :: (Char -> Bool) -> Parser Char
sat p = item >>>= \x -> 
           if p x then success x else failure

isDigit c = ('0' <= c && c <= '9')
digit = sat isDigit
--lower = sat isLower
--upper = sat isUpper
--letter = sat isAlpha
--alphaNum = sat isAlphaNum
char x = sat (== x)
