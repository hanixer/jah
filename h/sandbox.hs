module StateGame where

import Data.Bits
import Data.List (group, intersperse, find)
import Data.Char (ord, toUpper, isLetter)


numToBits n 
  | n == 0 = []
  | otherwise = (n .&. 1) : numToBits (shiftR n 1)

bitsToZeros bs = map (\b -> if b == 0 then "00" else "0") bs

groupAndShrink bs = 
  map (\x -> (head x, concat $ take (length x) $ repeat "0")) $ group bs

encodeString s = groupAndShrink $ bitsToZeros $
  concat $ map (fillEmptyZeros . reverse . numToBits . ord) s

fillEmptyZeros lst = (take (7 - (length lst)) $ repeat 0) ++ lst


toString lst = take ((length s) - 1) s
  where s = concat $ map (\(f,s) -> f ++ " " ++ s ++ " ") $ lst

final s = toString $ encodeString s


                         

solveWithString c t = resultToString $ solve c t

solve curr@(x,y) target@(xt,xy) 
  | curr == target = []
  | otherwise = let dv = diffVec curr target
                    (offset, s) = destination dv
                in s : (solve (summVec curr offset) target)

resultToString ss = concat $ intersperse "\n" ss

diffVec (x1,y1) (x2,y2) = (x2-x1,y2-y1)
summVec (x1,y1) (x2,y2) = (x1+x2,y1+y2)

destination (x,y)
  | x > 0 && y > 0 = ((1,1),"SE")
  | x > 0 && y < 0 = ((1,-1),"NE")
  | x > 0 && y == 0 = ((1,0),"E")
  | x < 0 && y > 0 = ((-1,1),"SW")
  | x < 0 && y < 0 = ((-1,-1), "NW")
  | x < 0 && y == 0 = ((-1,0), "W")
  | x == 0 && y > 0 = ((0,1),"S")
  | x == 0 && y < 0 = ((0,-1),"N")
  | otherwise = error "unexpected x,y pair"


stringToAscii :: String -> Int -> Int -> [String] -> String
stringToAscii s w h alphabet = concat $ intersperse "\n" $  combine $
  map (\c -> extractLetter (charToPos c) w alphabet) s

extractLetter pos w alphabet = map (\line -> take w $ drop (pos * w) line) alphabet

combine letters = foldl1 (\acc curr -> zipWith (++) curr acc) (reverse letters)

charToPos :: Char -> Int
charToPos c 
  | isLetter c = (ord $ toUpper c) - (ord 'A')
  | otherwise = (charToPos 'Z') + 1
--neighbs node@(bx,y) = [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]
                    
combinel xs = zip xs (drop 1 xs)

