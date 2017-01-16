{-# LANGUAGE DataKinds #-}
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.List as L
import Control.Monad (foldM)
import Prelude hiding ((*>))




type Pos = Int
type PosSet = IS.IntSet
type ILabel = Int -- identifies recognizer in memotable
type LeftRecCtxt = [(Pos,[(ILabel,Int)])]

 -- memotable : (recognizer label) -> start position -> result set
type State nodeName = IM.IntMap (IM.IntMap PosSet)
data StateM s t = StateM {runState :: s -> (t, s)} 
type NontermCuts = [(ILabel, Pos)]
type UpResult = (NontermCuts, PosSet)
type R memoLabel = LeftRecCtxt -> Pos -> StateM (State memoLabel) UpResult

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

term :: String -> R l
term x _ pos 
  | pos >= inputL = result IS.empty
  | input !! pos == x = result $ IS.singleton $ pos + 1
  | otherwise = result IS.empty
  where result set = return ([], set)

(<+>) :: R l -> R l -> R l
p1 <+> p2 = \lrCtxt pos -> do
  (cuts1, posSet1) <- p1 lrCtxt pos
  (cuts2, posSet2) <- p2 lrCtxt pos
  return (L.union cuts1 cuts2, IS.union posSet1 posSet2)

(*>) :: R l -> R l -> R l
p1 *> p2 = p
  where p lrCtxt pos = do
          (cuts1, posSet1) <- p1 lrCtxt pos
          foldM parseNextPos (cuts1, IS.empty) (IS.toList posSet1)
          where parseNextPos (resultCuts, resultPosSet) nextPos = do
                  (cuts2, posSet2) <- p2 lrCtxt nextPos
                  return $ (L.union resultCuts cuts2, IS.union resultPosSet posSet2)

memoize :: Enum l => l -> R l -> R l
memoize lbl p = \lrCtxt pos -> do
  memo <- get
  let isExists = isStoreExists memo pos
  return ([], IS.empty)
  where intLbl = fromEnum lbl 
        isStoreExists memo pos = 
          if IM.member intLbl memo
          then IM.member intLbl (memo IM.! intLbl)
          else False
          
{-
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

parse p = p 0

-- Test grammar
sA = term "a" *> term "b" <+> term "c"

data MyLabels = A|B|C deriving (Show, Enum)

bet = do
  m <- get
  return (IM.insert 1 (IM.singleton 2 3))
  
testft pos = fst $ runState (sA pos) IM.empty

exampleTable = IM.singleton 1 (IM.fromList [(1,(S.singleton 2)), (2,S.singleton 3)])

labeledTerm = memoize A sA
testlt pos = runState (labeledTerm pos) IM.empty
-}
-- Input
input = ["a","b","c","b"]
inputL = length input

test rec = do
     putStrLn ("LeftRec: " ++ (show lrCtxt))
     putStrLn ("PosSet: " ++ (show posSet))
     putStrLn ("Memo: " ++ (show memo))
  where ((lrCtxt, posSet), memo) = runState (rec [] 0) IM.empty 
