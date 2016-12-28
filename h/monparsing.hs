type Parser a = String -> [(a, String)]

result :: a -> Parser a
result v = \inp -> [(v, inp)]

zero :: Parser a
zero = \inp -> []

item :: Parser Char
item [] = [] 
item (x:xs) = [(x, xs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> case p inp of
  [] -> []
  [(v, rest)] -> (f v) rest
  _ -> []

sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` \x1 -> 
  if (p x1) then (result x1) else zero  

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

plus :: Parser a -> Parser a -> Parser a
p `plus` q = \inp -> (p inp ++ q inp)

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

threechar = letter `bind` \c1 ->
                            letter `bind` \c2 ->
                                            letter `bind` \c3 -> result [c3, c2, c1]
word :: Parser String
word = (letter `bind` \c -> 
           word `bind` \xs -> result (c:xs)) 
       `plus` (result "")

union :: Parser a -> Parser a -> Parser a
p `union` q = \inp -> case p inp of
  [] -> q inp
  [(v, inp)] -> [(v, inp)]


word2 = neWord `union` result ""
  where
    neWord = letter `bind` \x  ->
             word2   `bind` \xs -> result (x:xs)

