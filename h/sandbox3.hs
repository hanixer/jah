import qualified Data.Map as Map
import qualified Data.Array as I
import Data.Array ((!))
import System.IO.Unsafe
import Data.IORef

--memoFun :: (Ord a) => (a -> b) -> a -> b
memoFun f = unsafePerformIO $ do
  mapRef <- newIORef Map.empty
  return $ \a -> unsafePerformIO $ do
  currMap <- readIORef mapRef
  let vM = Map.lookup a currMap
  case vM of
    Just b -> return currMap
    Nothing -> do
      let b = f a
      writeIORef mapRef $ Map.insert a b currMap
      return currMap


f x = if x > 10  then 5 else 25

g = memoFun f 


stro a b = d (length a) (length b)
  where d i 0 = i
        d 0 j = j
        d i j 
          | a !! (i - 1) == b !! (j - 1) = d (i - 1) (j - 1)
          | otherwise = minimum [ d (i - 1) j + 1
                                , d i (j - 1) + 1
                                , d (i - 1) (j - 1) + 1
                                ]


stro2 a b = d m n
  where d i 0 = i
        d 0 j = j
        d i j 
          | a !! (i - 1) == b !! (j - 1) = ds ! ((i - 1), (j - 1))
          | otherwise = minimum [ ds ! ((i - 1), j) + 1
                                , ds ! (i, (j - 1)) + 1
                                , ds ! ((i - 1), (j - 1)) + 1
                                ]

        (m,n) = ((length a),length b)
        bounds = ((0,0),(m,n))
        ds = I.listArray bounds
          [d i j | (i, j) <- I.range bounds]
