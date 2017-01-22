import Data.Char (ord, chr)
import Data.Ix (inRange)
 
cipher :: Int -> String -> String
cipher k = map f
  where
    f c
      | inRange ('a','z') c = tr 'a' k c
      | inRange ('A','Z') c = tr 'A' k c
      | otherwise = c
 
uncipher :: Int -> String -> String
uncipher k = cipher (-k)
 
tr :: Char -> Int -> Char -> Char
tr base offset char = chr $ ord base + (ord char - ord base + offset) `mod` 26

main :: IO ()
main = putStrLn $ uncipher 1 "tufqifo.n.ejfim@hnbjm.dpn"
