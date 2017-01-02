-- IO (Maybe Integer Stack)
-- I can't find the code currently I am basing this off of, but I wanted to be
-- able to learn more about using StateT along with Maybe types.
import Control.Monad.State

main :: IO ()
main = runStateT code Nothing >> return ()

code :: StateT (Maybe [Integer]) IO ()
code = do
  push 1
  push 2
  push 3
  w <- pop
  io $ print w
  x <- pop
  io $ print x
  y <- pop
  io $ print y
  z <- pop
  io $ print z
  push 4
  zz <- pop
  io $ print zz
  return ()
 
pop :: StateT (Maybe [Integer]) IO (Maybe Integer)
pop = do
  xs <- get
  case fmap length xs of
    Nothing -> return Nothing
    Just 0 -> do
      put Nothing
      return Nothing
    _      -> do
      put (fmap tail xs)
      return (fmap head xs)
    
push :: Integer -> StateT (Maybe [Integer]) IO ()
push x = do
 xs <- get
 if xs == Nothing 
   then put (Just [x])
   else put (fmap (x:) xs)
 
io :: IO a -> StateT (Maybe [Integer]) IO a
io = liftIO
