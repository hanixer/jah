module Sandbox where

import qualified Data.IntMap as D

data Thing = Th (Int -> Char)

f x = if x > 1 then 'a' else '5'

th = Th f

m = D.fromList [(1,'a'),(2,'b')]
