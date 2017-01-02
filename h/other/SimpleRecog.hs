import qualified Data.IntSet as S
import qualified Data.IntMap as IM
import Prelude hiding ((*>))

type Pos = Int
type PosSet = S.IntSet
type R = Pos -> PosSet

 -- identifies recognizer in memotable
type ILabel = Int
 -- memotable : (recognizer label) -> start position -> result set
type State nodeName = IM.IntMap (IM.IntMap PosSet)
data StateM s t = StateM {runState :: s -> (t, s)} 
type RM memoLabel = Pos -> StateM (State memoLabel) PosSet

instance Functor (StateM s) where
  fmap g m = StateM $ \s -> let (a, s2) = runState m s 
                                mb = g a
                            in (g a, s2)

instance Applicative (StateM s) where
  pure a = StateM (\s -> (a, s))
  g <*> m = StateM (\s -> let (a, sa2) = runState m s
                              (h, sg2) = runState g s
                          in (h a, sa2))
instance Monad (StateM s) where
  return = pure
  m >>= g = StateM (\s -> let (a, s2) = runState m s
                              mb = g a
                          in runState mb s2)

get = StateM $ \s -> (s, s)

term :: String -> RM l
term x pos 
  | pos >= inputL = return S.empty
  | input !! pos == x = return $ S.singleton $ pos + 1
  | otherwise = return S.empty 

(<+>) :: RM l -> RM l -> RM l
p <+> q = \pos -> do
  posSetP <- p pos
  posSetQ <- q pos
  return $ S.union posSetP posSetQ

(*>) :: RM l -> RM l -> RM l
p *> q = \pos -> do
  posSetP <- p pos
  posSetsQ <- mapM q (S.elems posSetP)
  return $ S.unions posSetsQ

lookupMT :: ILabel -> Pos -> State l -> Maybe PosSet
lookupMT lab pos table = do
  posMap <- IM.lookup lab table
  res <- IM.lookup pos posMap
  return res

updateMT :: ILabel -> Pos -> PosSet -> State l -> State l
updateMT lab pos value table = 
  IM.alter updateSubtable lab table
  where updateSubtable Nothing = return $ IM.singleton pos value
        updateSubtable (Just subtable) = 
          return $ IM.insertWith (\old new -> S.union old new)
            pos value subtable

memoize :: Enum l => l -> RM l -> RM l
memoize eLabel parser = \pos -> do
  memTable <- get
  case lookupMT iLabel pos memTable of
    (Just res) -> return res
    Nothing -> 
      parser pos >>= \res ->
      StateM $ \_ -> (res, updateMT iLabel pos res memTable)
  where iLabel = fromEnum eLabel

-- Input
input = ["c", "b", "a"]
inputL = length input

parse p = p 0

-- Test grammar
--sA = term "a" *> term "b" <+> term "c"

data MyLabels = A|B|C deriving (Show, Enum)

bet = do
  m <- get
  return (IM.insert 1 (IM.singleton 2 3))
  
testft pos = fst $ runState (term "a" pos) IM.empty

exampleTable = IM.singleton 1 (IM.fromList [(1,(S.singleton 2)), (2,S.singleton 3)])

labeledTerm = memoize A (term "a")
testlt pos = runState (labeledTerm pos) IM.empty
