import qualified Data.ByteString.Lazy as L

hasElf :: L.ByteString -> Bool
hasElf content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return (hasElf content)

main = do
  contents <- getContents
  print (sumFile contents)
 where sumFile = sum . map read . words
