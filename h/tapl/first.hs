import Data.List (concat, nub)

data Term = Truen
          | Falsen
          | Zero
          | If Term Term Term
          | Succ Term
          | Pred Term
          | IsZero Term
  deriving (Show, Eq)

sets :: Int -> [Term]
sets 0 = []
sets 1 = [Truen, Falsen, Zero]
sets i = (nub . concat) [[If t1 t2 t3, Succ t1, Pred t1, IsZero t1] | t1 <- prev, t2 <- prev, t3 <- prev]
  where prev = sets (i - 1)

setsNum :: Int -> Int
setsNum 0 = 0
setsNum 1 = 3
setsNum i = 3 * prev + prev * prev * prev + 3
  where prev = setsNum (i - 1)

isNumericVal :: Term -> Bool
isNumericVal Zero = True
isNumericVal (Succ t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal Truen = True
isVal Falsen = True
isVal t = isNumericVal t

eval :: Term -> Term
eval Truen = Truen
eval Falsen = Falsen
eval Zero = Zero
eval (If Truen t _) = t
eval (If Falsen _ t) = t
eval (If t1 t2 t3) = let cond = eval t1 in
  If cond t2 t3
eval (Succ Zero) = Succ Zero
eval (Succ t@(Succ _)) = Succ t
eval (Succ t) = let t' = eval t in
  Succ t'
eval (Pred Zero) = Zero
eval (Pred (Succ nv)) = nv
eval (Pred t) = let t' = eval t in
  Pred t'
eval (IsZero Zero) = Truen
eval (IsZero (Succ nv)) 
  | isNumericVal nv = Falsen
  | otherwise = error "expected something like number"
eval (IsZero t) = IsZero $ eval t
