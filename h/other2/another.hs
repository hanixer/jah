import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Hashable as HS
import Control.Monad.State

--------------------------------------------------
-- Memoization
--------------------------------------------------
memoFn :: (Ord a) => (a -> b) -> (a -> b)
memoFn f = unsafePerformIO $ do
  mapRef <- newIORef Map.empty
  return $ \a -> unsafePerformIO $ do
    currMap <- readIORef mapRef
    let storVal = Map.lookup a currMap
    case storVal of
      Just b -> return b
      Nothing -> do
        let b = f a
        writeIORef mapRef $ Map.insert a b currMap
        return b

--------------------------------------------------
-- Parsing 
--------------------------------------------------    
data Result a = Success a String
              | Failure String
  deriving (Show)

type Parser a = String -> Result a

succeed :: a -> Parser a
succeed val = \str -> Success val str

failure = Failure

string :: String -> Parser String
string pattern str =
  if start == pattern
  then Success start rest
  else Failure str
  where len = length pattern
        (start, rest) = (take len str, drop len str)

alt :: Parser a -> Parser a -> Parser a
alt p q = \str ->
  case p str of
    s@(Success val rest) -> s
    _ -> q str

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p fn = \str ->
  case p str of
    (Success val rest) -> (fn val) rest
    (Failure s) -> Failure s

sequ :: Parser a -> Parser b -> Parser (a,b)
sequ p q = 
  bind p (\v1 -> bind q (\v2 -> succeed (v1,v2))) 

--------------------------------------------------
-- Using continuations
--------------------------------------------------
succeed' val = \str cont -> cont $ Success val str

string' pattern str cont =
  if start == pattern
  then cont $ Success start rest
  else cont $ Failure str
  where len = length pattern
        (start, rest) = (take len str, drop len str)

bind' p fn str cont = p str myCont
  where myCont (Success val rest) = (fn val) rest cont
        myCont f@(Failure _) = cont f

sequ' p q = 
  bind' p (\v1 -> bind' q (\v2 -> succeed' v2))

alt' p q = \str cont -> (p str cont, q str cont)

data ParseEq = PEStr Int
             | PEAlt ParseEq ParseEq
             | PESeq ParseEq ParseEq
  deriving (Show, Eq)

type Parserr a b c = (Int, String -> (a -> b) -> c)

succeedN val = (HS.hash [[[val]]], \str cont -> cont $ Success val str)

stringN pattern =
  (hsh, p)
  where hsh = HS.hash $ C.pack pattern
        p str cont = 
            if start == pattern
            then cont $ Success start rest
            else cont $ Failure str
            where len = length pattern
                  (start, rest) = (take len str, drop len str)

bindN (pHash, p) fn = (qHash, q)
    where qHash = HS.hash [pHash]
          q str cont = p str myCont
            where myCont (Success val rest) = (fn val) rest cont
                  myCont f@(Failure _) = cont f

altN (p1Hash, p1) (p2Hash, p2) = (pHash, p)
  where pHash = HS.hash [[p1Hash, p2Hash]]
        p str cont = (p1 str cont, p2 str cont)

sequN p1 p2 = 
  undefined
  --bindN p1 (\v1 -> bindN p2 (\v2 -> succeedN v2))

parse (_, p) str cont = p str cont

getNextId :: State Int Int
getNextId = do
  n <- get
  put (n + 1)
  return $ (n + 1)

--succeedN :: a -> 
succeedM val = do
  n <- getNextId
  return $ (n, \str cont -> cont $ Success val str)

stringM pattern = do
  n <- getNextId
  return (n, p)
  where p str cont = 
          if start == pattern
          then cont $ Success start rest
          else cont $ Failure str
            where len = length pattern
                  (start, rest) = (take len str, drop len str)

bindM (_, p1) fn = do
  n <- getNextId
  return (n, p)
  where p str cont = p1 str (\r -> case r of
          (Success val rest) -> (fn val) rest cont
          f@(Failure _) -> cont f)
        --where myCont (Success val rest) = (fn val) rest cont
              --myCont f@(Failure _) = cont f

altM (_, p1) (_, p2) = do
  n <- getNextId
  return (n, p)
  where p str cont = (p1 str cont, p2 str cont)

altMS s1 s2 = do
  

p1 = do
  a <- stringM "a"
  b <- stringM "b"
  a1 <- altM a b
  cc <- stringM ""
  altM a1 cc

getParser m = evalState m 0
--------------------------------------------------
-- Examples
-------------------------------------------------
article = alt (string "the ") (string "a ")
noun =  alt (string "student ") (string "professor ")
verb = alt (string "studies ") (string "lectures ")
nounPhrase = sequ article noun
verbPhrase = sequ verb nounPhrase 
sentence = sequ nounPhrase verbPhrase


