--
-- Basic demonstration of algorithm
--
-- it builds trees rather than semantic results
--
import Prelude as P hiding ((*>))
import Control.Monad(foldM)
import Data.Monoid hiding ((<>))
import Data.IntSet as S 
import Data.IntMap as I
import Text.PrettyPrint.HughesPJ hiding ((<+>)) 
infixl 3 <+>
infixl 4 *>
data Tree l a 
 = Leaf a
 | Branch [Tree l a] 
 | SubNode l (Start,End)
   deriving (Show, Eq, Ord)           
class Enum l => MemoLabel l where
data Tag a = Tag !Int a deriving Show
newtype E s = E [Tag [s]] deriving (Show)
instance Functor E where
      fmap f (E ts) = E [ Tag i (fmap f x) | Tag i x <- ts ]
instance Monoid (E a) where
      mempty              = E []   
      mappend (E a) (E b) = E $ sorted_merge a b
              
sorted_merge :: Monoid a => [Tag a] -> [Tag a] -> [Tag a]
sorted_merge [] bs = bs
sorted_merge as [] = as
sorted_merge as@(ia@(Tag i a):at) bs@(jb@(Tag j b):bt)
 = case i `compare` j of
      LT -> ia : sorted_merge at bs
      EQ -> Tag i (mappend a b) : sorted_merge at bt
      GT -> jb : sorted_merge as bt
type Start = Int
type End   = Int
type Pos = Int
type ILabel = Int
type Result memoLabel      = [Tree memoLabel String] 
type ParseResult memoLabel = E (Result memoLabel)
data Stored memoLabel 
 = Stored { s_stored  :: UpResult memoLabel   
          , s_context :: L_Context            
          , s_results :: E (Tree memoLabel String) 
          }
type State  memoLabel = IntMap {-ILabel-} (IntMap (Stored memoLabel))
type L_Context     = [(ILabel, Int)]  
type CurtailingNTs = IntSet
empty_cuts = S.empty
type UpResult memoLabel = (CurtailingNTs, ParseResult memoLabel)
type P memoLabel 
 = L_Context -> Pos -> StateM (State memoLabel) (UpResult memoLabel)
--
-- state monad
-- 
newtype StateM s t = State { unState :: s -> (t, s) }
instance Functor (StateM s) where
  fmap f m = m >>= \x -> return (f  x)
instance Applicative (StateM s) where
  pure = return
  f <*> m = f >>= \g -> m >>= \x -> (State (\s -> (g x, s)))
instance Monad (StateM s) where
      return x = State (\t -> (x,t))
      State m >>= k 
       = State (\s -> let (a,y) = m s in unState (k a) y)
get       :: StateM s s
get        = State $ \s -> (s,s)
put       :: s -> StateM s ()
put s      = State $ \_ -> ((),s)
modify    :: (s -> s) -> StateM s ()
modify sf  = State $ \s -> ((), sf s)
--
-- combinators
-- 
emptyP :: P a 
emptyP cc inp = return (empty_cuts, E [Tag inp [[Leaf "0"]]])
term :: String -> P l
term c cc inp 
 | inp == length input  = return (empty_cuts, mempty)
 | input !! inp == c    = return (empty_cuts, E [Tag (inp+1) [[Leaf c]]])
 | otherwise            = return (empty_cuts, mempty)
(<+>) :: P l -> P l -> P l
(p <+> q) inp cc 
 = do
      (cut1,m) <- p inp cc  
      (cut2,n) <- q inp cc
      return ( S.union cut1 cut2
             , m `mappend` n )
(*>)  :: P l -> P l -> P l
(p *> q) cc inp
 = do
      (cut, E p_results) <- p cc inp 
      let 
	  addP :: [Result l] -> ParseResult l -> ParseResult l
	  addP left_result (E rights)
	   = E [ Tag re [ r ++ l | l <- left_result, r <- right_results ]
	       | Tag re right_results <- rights ]
          join_with_first (Tag end l_results)         
           = do 
                let null_p = end == inp
                let new_ctxt | null_p     = cc                
                             | otherwise  = []                
                (new_cuts, q_result) <- q new_ctxt end
                let out_cuts | null_p    = new_cuts `S.union` cut  
                             | otherwise = cut                     
                return ( out_cuts , addP l_results q_result )
          join_with_rest prev_results (Tag end l_results) 
           = do 
                (_, q_result) <- q [] end
                return $ addP l_results q_result `mappend` prev_results
      case p_results of
        []   -> return (cut, mempty)
        r:rs -> do
                   (cuts,first_result) <- join_with_first r
                   results             <- foldM join_with_rest first_result rs
                   return ( cuts , results )
memoize :: MemoLabel l => l -> P l -> P l 
memoize e_name f context pos
 = 
   do
      mTable <- get
      case lookupT i_name pos context mTable of       
        Just res 
             -> return res
        Nothing 
             | funccount context > length input - pos + 1
             -> 
                return (S.singleton i_name, E [] {-mempty-})
             | otherwise
             -> do
                  let new_down_ctxt = incContext i_name context 
                  (up_ctxt,results) <- f new_down_ctxt pos
                  let subnodes = addNode e_name pos results
                  let result   = (up_ctxt,subnodes) 
                  let to_branch [x] = x
                      to_branch xs  = Branch $ reverse xs
                      
                      
                  let repacked = E [ Tag e $ fmap to_branch ts 
                                   | Tag e ts <- (\(E x) -> x) results ]
                              
                  let sub_ctxt = pruneContext up_ctxt context
                  let to_store = Stored result sub_ctxt repacked
                  modify (update_ to_store)
                  return result
   where 
      i_name = fromEnum e_name
      update_ :: Stored l -> State l -> State l
      update_ res 
       = I.insertWith (\_ prev -> I.insert pos res prev) 
                      i_name 
                      (I.singleton pos res)
      funccount cs = case P.lookup i_name cs of
                       Nothing -> 0
                       Just fc -> fc 
addNode name inp (E rs)
 = E [ Tag e [[SubNode name (inp,e)]] | Tag e _ <- rs ]
pruneContext :: CurtailingNTs -> L_Context -> L_Context
pruneContext rs _
 | S.null rs = []
pruneContext rs ctxt
 = [ nc | nc@(n,c) <- ctxt, n `S.member` rs ]
   
incContext :: ILabel -> L_Context -> L_Context 
incContext name []                         = [(name,1)]
incContext name (nc@(n,c):ncs) | n == name = (n,c + 1) : ncs 
                               | otherwise = nc : incContext name ncs
                           
lookupT :: ILabel -> Int -> L_Context -> State l -> Maybe (UpResult l)
lookupT name inp current mTable
 = do
      inner <- I.lookup name mTable
      memo  <- I.lookup inp inner
      let stored = s_stored memo
      if S.null (fst stored)
        then return stored            
        else if canReuse current (s_context memo) 
               then Just stored
               else Nothing
   where
      canReuse :: L_Context -> L_Context -> Bool
      canReuse current stored 
       = and [ or [ cc >= sc 
                  | (cn,cc) <- current, sn == cn ]
             | (sn,sc) <- stored ]
      
test :: MemoLabel l => P l -> Int -> State l
test p i = snd (unState (p [] i) I.empty)
--
-- sample input and grammar
--
input :: [String]
input = replicate 4 "a"
data L = A | B | C deriving (Show, Eq, Ord, Enum, Bounded)
instance MemoLabel L where
instance PP L where pp l = text $ show l
sI = memoize A $     term "a" *> sI *> sI
                 <+> sI *> sI *> term "a"
                 <+> emptyP *> sI *> sI *> term "a"
                 <+> opt_x *> sI *> sI *> term "a"
                 <+> sJ *> sI *> term "a"
                 <+> sK *> sJ *> term "a"	-- lr chain in 2nd pos
		 <+> term "a"
sJ = memoize B $     sI 
                 <+> sJ *> term "a"
sK = memoize C $     sI 
                 <+> sJ *> term "a"
opt_x = term "x" <+> emptyP		-- NB not memoized
--
-- testing and nice output
-- 
-- "qt" shows all memo results starting from the given input position
qt :: Int -> IO ()
qt n = putStrLn $ render $ format format_result toEnum $ test sI n
class PP a where
      pp :: a -> Doc
format_table toEnum s@(Stored (cs,E sn) ct rs)
 = vcat [ text "CUT: " <> pp (P.map (pp.toEnum) $ S.toAscList cs)
        , text "CTX: " <> pp [ (toEnum c, j) | (c,j) <- ct ]
        
        , text "RES: " <> format_result toEnum s
        ]
format_result :: PP l => (Int -> l) -> Stored l -> Doc
format_result _ (Stored _ _ (E rs))
 = pp_list sep rs
format_top :: (PP l, MemoLabel l) => l -> State l -> Doc
format_top root table 
 = maybe (text "") id 
 $ do
      inner  <- I.lookup (fromEnum root) table
      result <- I.lookup 0               inner
      return $ format_result (error "") result
format :: (PP l, MemoLabel l) 
       => ((Int -> l) -> Stored l -> Doc) 
       -> (Int -> l) 
       -> State l
       -> Doc
format inner_format toEnum t 
 = vcat
   [ text "Label " <> pp (toEnum s)
     $$ 
     nest 1 (pp_list vcat
                [ pp i <> text " -> " <> inner_format toEnum inner 
                | (i,inner) <- I.toList sr
                , let (E results) = s_results inner
                , not $ P.null results
                ])
   | (s,sr) <- I.toList t ]
instance PP Doc where
      pp c = c
instance PP Int where
      pp i = text $ let s = show i in replicate (3 - length s) ' ' ++ s
instance (PP a) => PP (Tag a) where
      pp (Tag a b) = pp (a,b)
instance (PP a, PP b) => PP (a,b) where
      pp (a,b) = pp a <> text " -> " <> pp b
instance PP a => PP [a] where
      pp = pp_list sep
instance (Show t, PP l) => PP (Tree l t) where
      pp (Leaf x)          = text "Leaf "    <> text (show x)
      pp (Branch ts)       = text "Branch "  <> pp_list sep ts
      pp (SubNode x (s,e)) = text "SubNode " <> pp x <> text (show (s,e)) 
pp_list cat_or_sep []     = brackets $ text ""
pp_list cat_or_sep (x:xs) = cat_or_sep $ (char '[' <> pp x)
                                       : [ comma <> pp y | y <- xs ]
                                       ++ [char ']']
instance PP a => PP (IntMap a) where
	pp = pp_list cat . I.toAscList
