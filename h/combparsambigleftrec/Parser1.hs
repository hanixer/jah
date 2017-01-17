{-# LANGUAGE DataKinds #-}
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.List as L
import Control.Monad (foldM)
import Prelude hiding ((*>))
import Data.Maybe (fromJust)

type Pos = Int
type PosSet = IS.IntSet
type ILabel = Int -- identifies recognizer in memotable
type LeftRecCtxt = [(Pos,[(ILabel,Int)])]

type NontermCuts = [(ILabel, Pos)]
type UpResult = (NontermCuts, PosSet)
type Stored = (LeftRecCtxt, UpResult)
type State nodeName = IM.IntMap (IM.IntMap Stored)
data StateM s t = StateM {runState :: s -> (t, s)} 
type R memoLabel = LeftRecCtxt -> Pos -> 
                   StateM (State memoLabel) UpResult

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
put s      = StateM $ \_ -> ((),s)

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
  if canReuse memo intLbl pos lrCtxt
  then return $ snd $ fromJust $ lookupMemo memo intLbl pos
  else if shouldCut lrCtxt pos intLbl
       then return ([(intLbl, pos)], IS.empty)
       else do
        let lrCtxt' = incCounter lrCtxt pos intLbl
        result@(cuts, posSet) <- p lrCtxt' pos
        memo <- get
        saveResults memo result lrCtxt' pos intLbl
        return result
  where intLbl = fromEnum lbl 
        isStoreExists memo pos = 
          if IM.member intLbl memo
          then IM.member intLbl (memo IM.! intLbl)
          else False

canReuse memo lbl pos ctxtCurr = 
  case lookupMemo memo lbl pos of
    (Just (ctxtStored, _)) -> canReuseCtxt ctxtStored ctxtCurr
    Nothing -> False

canReuseCtxt :: LeftRecCtxt -> LeftRecCtxt -> Bool
canReuseCtxt stored current = 
  all (\(p1,labels1) ->
          case L.lookup p1 current of
            (Just labels2) -> 
              compareLabels labels1 labels2
            Nothing -> True)  stored
  where compareLabels labels1 labels2 = 
          all (\(lbl1, count1) ->
                  case L.lookup lbl1 labels2 of
                    (Just count2) -> count1 <= count2
                    Nothing -> True) labels1

shouldCut :: LeftRecCtxt -> Pos -> ILabel -> Bool
shouldCut lrCtxt pos lbl = case lookupCtxt lrCtxt pos lbl of
  (Just count) -> count > (inputL - pos) + 1
  Nothing -> False
    
lookupMemo :: State l -> ILabel -> Pos -> Maybe Stored
lookupMemo memo lbl pos = do
  posMap <- IM.lookup lbl memo
  IM.lookup pos posMap

incCounter :: LeftRecCtxt -> Pos -> ILabel -> LeftRecCtxt
incCounter lrCtxt pos lbl = updateCtxt lrCtxt pos lbl (+1) 1

updateCtxt :: LeftRecCtxt -> Pos -> ILabel -> (Int -> Int) -> Int -> 
              LeftRecCtxt
updateCtxt lrCtxt pos lbl f init = go1 lrCtxt
  where go1 (x@(pos1, ys):xs) 
          | pos == pos1 = (pos, go2 ys):xs
          | otherwise = x : go1 xs
        go1 [] = [(pos, [(lbl, init)])]
        go2 (y@(lbl1, count):ys)
          | lbl1 == lbl = (lbl, f count):ys
          | otherwise = y : go2 ys
        go2 [] = [(lbl, init)]

saveResults :: State l -> UpResult -> LeftRecCtxt -> Pos -> ILabel -> 
               StateM (State l) ()
saveResults memo upres@(cuts, posSet) lrCtxt pos lbl = do
  put updateMemo
  where ctxtToStore = filterCtxt lrCtxt cuts
        updateMemo = IM.alter posMap lbl memo
        posMap (Just pmap) = 
          Just $ IM.insert pos (ctxtToStore, upres) pmap
        posMap Nothing = Just $ IM.singleton pos (ctxtToStore, upres)
  

filterCtxt :: LeftRecCtxt -> NontermCuts -> LeftRecCtxt
filterCtxt lrCtxt cuts = 
  foldl (\newCtxt (pos,lbl) ->
           case lookupCtxt lrCtxt pos lbl of
             (Just count) -> updateCtxt newCtxt pos lbl id count
             Nothing -> newCtxt) [] cuts

lookupCtxt :: LeftRecCtxt -> Pos -> ILabel -> Maybe Int
lookupCtxt ctxt pos lbl = do
  labels <- L.lookup pos  ctxt
  L.lookup lbl labels

-- Input
input = replicate 4 "a"
inputL = length input

test rec = do
     putStrLn ("LeftRec: " ++ (show lrCtxt))
     putStrLn ("PosSet: " ++ (show posSet))
     putStrLn ("Memo: " ++ (IM.showTree memo))
  where ((lrCtxt, posSet), memo) = runState (rec [] 0) IM.empty 

-- Test Grammar
data L = A | B | C deriving (Show, Eq, Ord, Enum, Bounded)
sS = memoize A $ sS *> sS <+> term "a"
