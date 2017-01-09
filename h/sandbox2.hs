import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.IntMap as IM

type Node = Int
type NodeSet = IS.IntSet
type NodeQ = Queue Node
type Gateways = [Int]
type Link = IS.IntSet
type LinkSet = S.Set Link

--bfs :: Node -> Node -> NodeQ -> NodeSet -> IM.IntMap Node -> [Link] -> [Node]
--bfs curr target queue seen parents links =
--  where nextNodes = 

bfs start target links = 1
  where bfs' :: Node -> NodeQ -> NodeSet -> NodeSet -> IM.IntMap Node -> [Node]
        bfs' curr queue seen visited parents =
          if curr == target
          then backtrack curr parents
          else error ""

nextNodes curr links seen = 
  S.filter (\l -> (IS.member curr l) && not (IS.member 

backtrack curr parents = case IM.lookup curr parents of 
  (Just p) -> curr : (backtrack p parents)
  Nothing -> []


--------------------------------------------------
-- Queue
--------------------------------------------------
data Queue a = Queue { front :: [a], back :: [a] }
  deriving (Show)

emptyQ = Queue [] []

isEmpty (Queue f b) = f == [] && b == []

enqueue (Queue f b) x = (Queue f (x:b))

dequeue (Queue [] []) = error "nothing to dequeue"
dequeue (Queue [] xs) = dequeue (Queue (reverse xs) [])
dequeue (Queue [x] b) = (x, Queue (reverse b) [])
dequeue (Queue (x:xs) b) = (x, Queue xs b)
  
