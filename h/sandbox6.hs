import Data.List ((\\))

minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

--(\\\) :: Eq a => [a] -> [a] -> [a]
--xs \\\ ys = filter (\x -> not $ elem x ys) xs

f 0 
  | True = False
  | otherwise = otherwise
  where x = 5 + 6