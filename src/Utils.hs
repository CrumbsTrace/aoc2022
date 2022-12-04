module Utils (readFromFile, runParser, sortDesc, splitInHalf, inBounds) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, parseOnly)
import Data.ByteString qualified as BS
import Data.List ( sortOn )
import Data.Ord (Down (Down))

readFromFile :: FilePath -> IO BS.ByteString
readFromFile = BS.readFile

runParser :: Parser [a] -> BS.ByteString -> [a]
runParser p b = case parseOnly p b of
  Left _ -> []
  Right s -> s

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortOn Down

splitInHalf :: [Int] -> ([Int], [Int])
splitInHalf s = splitAt (length s `div` 2) s

inBounds :: Int -> (Int, Int) -> Bool
inBounds c (b1, b2) = b1 <= c && b2 >= c

