import Data.List ((\\))

minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

--(\\\) :: Eq a => [a] -> [a] -> [a]
--xs \\\ ys = filter (\x -> not $ elem x ys) xs