import qualified Data.IntSet as S
import qualified Data.IntMap as IM

type Pos = Int
type PosSet = S.IntSet
type R = Pos -> PosSet

type ILabel = Int
type RM memoLabel = Pos -> StateM (State memoLabel) PosSet
data StateM s t = State {unState :: s -> (t, s)}
type State nodeName = IM.IntMap (IM.IntMap PosSet)

instance Functor (StateM s) where
  fmap f x = State (\s -> (f (fst ((unState x) s)), s))

instance Applicative (StateM s) where
  pure x = State (\s -> (x, s))
  g <*> x = State (\s0 -> (
{-
(*>) :: RM l -> RM l -> RM l
p *> q = \r -> do endP <- p r
                  endQs <- mapM q (S.elems endP)
                  return $ S.unions endQs
-}
(<+>) :: R -> R -> R
p <+> q = \r -> S.union (p r) (q r)

--(*>) :: R -> R -> R
--p *> q = \r -> S.unions $ map q $ S.elems $ p r

parse :: R -> PosSet
parse p = p 0
