module Sandbox where

import Parser

data Maybee a = Juste a
              | Nothinge

divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x y = Just (x `div` y)

data Sheep = Sheep (Maybe Sheep) (Maybe Sheep) Int

createShip name = Sheep Nothing Nothing name

a = Sheep (Just (Sheep Nothing Nothing 1)) (Just (Sheep Nothing Nothing 2)) 4
b = Sheep Nothing (Just (Sheep Nothing Nothing 3)) 5
c = Sheep (Just a) (Just b) 6

matGrafa (Sheep m f n) = case m of
                           Nothing -> Nothing
                           (Just (Sheep m f n)) -> case f of
                                                     Nothing -> Nothing
                                                     (Just (Sheep m f n)) -> f

mother (Sheep m f n) = m
father (Sheep m f n) = f
num (Sheep _ _ n) = n



comb :: Maybe a -> (a -> b) -> Maybe b
comb Nothing _ = Nothing
comb (Just v) f = Just (f v)

--mgf s = comb s \(Sheep m f n) comb
           

mgsf s = (return s) >>= mother >>= father

type Zum = Int
data Da a = Da a
class Monj m where
  f :: m a -> Int

data Daa b a = Daa a a

data Tree = Tree (Maybe Tree) (Maybe Tree) Int

left (Tree l _ _) = l
right (Tree _ r _) = r
unbint Nothing = -1
unbint (Just (Tree _ _ n)) = n

llr t = (return t) >>= left >>= left >>= right

t = 
  Tree 
  (Just (Tree
   (Just (Tree Nothing (Just (Tree Nothing Nothing 8)) 4))
   (Just (Tree 
    (Just (Tree Nothing Nothing 9))
    (Just (Tree Nothing Nothing 10))
    5))
  2))
  (Just (Tree
   (Just (Tree Nothing Nothing 6))
   (Just (Tree (Just (Tree Nothing Nothing 11)) Nothing 7))
   3))
  1

n = parse (string "JIJIJ")    
