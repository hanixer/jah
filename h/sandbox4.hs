import Data.Map as Map
import System.IO.Unsafe
import Data.IORef

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
    
fib n = if n <= 1 then n else (fib (n-1)) + (fib (n-2))
mfib = memoFn (\n -> if n <= 1 then n else (mfib (n-1)) + (mfib (n-2)))
