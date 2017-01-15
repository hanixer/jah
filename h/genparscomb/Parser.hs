import qualified Data.IntMap as IMap
import Control.Monad.State

data Result a = Success a String
              | Failure String
  deriving (Show)

type Cont a = Result a -> ()
type Parser a = String -> Cont a -> ()

data ConstructionData a = 
  CD { cdTable :: IMap.IntMap (Parser a),
       cdLastId :: Int }

string pattern str cont =
  if start == pattern
  then cont $ Success start rest
  else cont $ Failure str
  where len = length pattern
        (start, rest) = (take len str, drop len str)

alt :: Parser a -> Parser a -> State Int (Parser a)
alt p1 p2 = do
  n <- get
  let m1 =  n + 1
  let m2 = m1 + 1
  put m2
  return (\str cont -> undefined)

bara = do
  p <- alt p (string "b")
  return p
