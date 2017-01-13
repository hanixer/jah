data Result a = Success a String
              | Failure String
  deriving (Show)

type Parser a = String -> Result a

succeed :: a -> Parser a
succeed val = \str -> Success val str

failure = Failure

string :: String -> Parser String
string pattern str =
  if start == pattern
  then Success start rest
  else Failure str
  where len = length pattern
        (start, rest) = (take len str, drop len str)

alt :: Parser a -> Parser a -> Parser a
alt p q = \str ->
  case p str of
    s@(Success val rest) -> s
    _ -> q str

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p fn = \str ->
  case p str of
    (Success val rest) -> (fn val) rest
    (Failure s) -> Failure s

sequ :: Parser a -> Parser b -> Parser (a,b)
sequ p q = 
  bind p (\v1 -> bind q (\v2 -> succeed (v1,v2))) 


article = alt (string "the ") (string "a ")
noun =  alt (string "student ") (string "professor ")
verb = alt (string "studies ") (string "lectures ")
nounPhrase = sequ article noun
verbPhrase = sequ verb nounPhrase 
sentence = sequ nounPhrase verbPhrase
  
