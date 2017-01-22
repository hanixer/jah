import Data.Time
import Control.Monad
import Control.Monad.State

type Var = String
type Val = String
data Predicate = Is Var Val
               | Equal Var Var
               | And Predicate Predicate
               | Or Predicate Predicate
               | Not Predicate
               deriving (Eq, Show)

type Vars = [(Var, Val)]

data ProblemState = PS {vars::Vars, constraints::[Predicate]}
  deriving (Show)

type NDS a = StateT ProblemState [] a

isNot var val = Not (Is var val)

implies a b = (Not a) `Or` b

orElse a b = (a `And` (Not b)) `Or` ((Not a) `And` b)

check :: Predicate -> Vars -> Maybe Bool
check (Is var val) vars = do
  val2 <- lookup var vars
  return (val == val2)
check (Equal var1 var2) vars = do
  val1 <- lookup var1 vars
  val2 <- lookup var2 vars
  return (val1 == val2)
check (And pred1 pred2) vars = do
  val1 <- check pred1 vars
  val2 <- check pred2 vars
  return (val1 && val2)
check (Or pred1 pred2) vars = do
  val1 <- check pred1 vars
  val2 <- check pred2 vars
  return (val1 || val2)
check (Not pred) vars = liftM (not) (check pred vars)


getVar :: Var -> NDS (Maybe Val)
getVar v = do
  vs <- gets vars
  return $ lookup v vs

setVar :: Var -> Val -> NDS ()
setVar v x = do 
  s <- get
  vs <- return $ filter ((v/=).fst) (vars s)
  put $ s {vars = (v,x):vs}

isConsistent :: Bool -> NDS Bool
isConsistent partial = do
  cs <- gets constraints
  vs <- gets vars
  let results = map (\p -> check p vs) cs
  return $ and (map (maybe partial id) results)

-- 
tps = PS [("a","b"),("b","a")] [(Not (Equal "a" "b"))]
