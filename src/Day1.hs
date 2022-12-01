module Day1 (run) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, sepBy')
import Data.ByteString (ByteString)
import Utils (runParser, sortDesc, bstring)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    parsedInput = runParser parser input
    sortedCalories = sortDesc $ map sum parsedInput
    p1 = head sortedCalories
    p2 = sum $ take 3 sortedCalories

parser :: Parser [[Int]]
parser = (decimal `sepBy'` char '\n') `sepBy'` bstring "\n\n"
