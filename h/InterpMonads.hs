type Name = String

data Exp = Lit Integer
         | Var Name
         | Minus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)
