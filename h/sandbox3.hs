{-# LANGUAGE FlexibleContexts #-}
import Data.Char (isUpper)

countupper s = foldl (\count c -> if isUpper c then count + 1 else count) 0 (reverse s)

data Stat = Stat { gl :: Int, sogl :: Int }
  deriving (Show)

countupp :: String -> (Int, Stat)
countupp [] = (0, Stat 0 0)
countupp (c:cs) = (count, stat)
  where (count, (Stat gl sogl)) = countupp cs
        count' = if isUpper c then count + 1 else count
        stat = if (c == 'a' || c == 'e') then (Stat (gl + 1) sogl) else (Stat gl (sogl + 1))

countu :: String -> Stat -> (Int, Stat)
countu [] = \s -> (0, s)
countu (c:cs) = \s -> if isUpper c then count + 1 else count
