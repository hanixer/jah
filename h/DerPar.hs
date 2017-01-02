data Lang = Empty
          | Eps
          | Charl Char
          | Cat {left :: Lang, right :: Lang}
          | Alt {this :: Lang, that :: Lang}
          | Rep {lang :: Lang}
  deriving (Show)

derivative :: Char -> Lang -> Lang
derivative _ Empty = Empty
derivative _ Eps = Empty
derivative c (Charl d) 
  | c == d = Eps
  | otherwise = Empty
derivative c (Alt this that) = 
  Alt (derivative c this) (derivative c that)
derivative c (Cat Eps right) = derivative c right
derivative _ (Cat Empty _) = Empty
derivative c (Cat left right) 
  | nullable left = derivative c right
  | otherwise = (Cat (derivative c left) right)
derivative c (Rep lang) = 
  Cat (derivative c lang) (Rep lang)
  
nullable :: Lang -> Bool
nullable Empty = False
nullable Eps = True
nullable (Charl _) = False
nullable (Rep _) = True
nullable (Alt this that) = 
  (nullable this) || (nullable that)
nullable (Cat left right) = 
  (nullable left) && (nullable right)

matches [] lang = nullable lang
matches (c:cs) lang = matches cs $ derivative c lang
