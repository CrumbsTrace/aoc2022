module Utils (
  readFromFile, 
  runParser, 
  sortDesc, 
  splitInHalf, 
  inBounds, 
  listToMap1, 
  skipLine) where

import Data.Attoparsec.ByteString.Char8 as P 
import Data.ByteString qualified as BS
import Data.List ( sortOn )
import Data.Ord (Down (Down))
import Data.Map.Strict qualified as Map

readFromFile :: FilePath -> IO BS.ByteString
readFromFile = BS.readFile

runParser :: Parser a -> BS.ByteString -> a
runParser p b = case parseOnly p b of
  Left _ -> error "rip"
  Right s -> s

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortOn Down

splitInHalf :: [Int] -> ([Int], [Int])
splitInHalf s = splitAt (length s `div` 2) s

inBounds :: Int -> (Int, Int) -> Bool
inBounds c (b1, b2) = b1 <= c && b2 >= c

listToMap1 :: [b] -> Map.Map Int b
listToMap1 = listToMapHelper Map.empty 1

listToMapHelper :: Map.Map Int b  -> Int -> [b] -> Map.Map Int b
listToMapHelper stacks _ [] = stacks
listToMapHelper stacks i (x:xs) = listToMapHelper (Map.insert i x stacks) (i + 1) xs

skipLine :: Parser ()
skipLine = skipWhile ('\n'/=) <* char '\n'
