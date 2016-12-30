module PNM where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
  greyWidth :: Int,
  greyHeight :: Int,
  greyMax :: Int,
  greyData :: L.ByteString
  } 

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                           " " ++ show m

matchHeader prefix str
  | prefix `L8.isPrefixOf` str
    = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
  | otherwise 
    = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
               | num <= 0 -> Nothing
               | otherwise -> Just (fromIntegral num, rest)

getBytes n str = let count = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both



parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of 
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                    case getBytes 1 s4 of
                      Nothing -> Nothing
                      Just (_, s5) ->
                        case getBytes (width * height) s5 of 
                          Nothing -> Nothing
                          Just (bitmap, s6) ->
                            Just (Greymap width height maxGrey bitmap, s6)

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
(Just v) >>? f = f v

parseP5_2 s =
  matchHeader (L8.pack "P5") s >>? \s1 ->
  getNat s1 >>? \(width, s2) -> 
  getNat (L8.dropWhile isSpace s2) >>? \(height, s3) ->
  getNat (L8.dropWhile isSpace s3) >>? \(maxGrey, s4) ->
  if maxGrey > 255 then Nothing else getBytes 1 s4 >>? \(_, s5) ->
  getBytes (width * height) s5 >>? \(bitmap, s6) -> 
    Just (Greymap width height maxGrey bitmap, s6)

testString = (L8.pack "P5 2 2 5 1 2 3 4")

data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Int
  } deriving (Show)

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
  }

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState 
  = case runParse parser (ParseState initState 0) of
      Left err -> Left err
      Right (result, _) -> Right result

modifyOffset :: ParseState -> Int -> ParseState
modifyOffset initState newOffset = 
  initState { offset = newOffset }
