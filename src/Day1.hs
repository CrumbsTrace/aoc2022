module Day1 (run) where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, sepBy')
import Data.ByteString (ByteString)
import Utils (runParser, sortDesc)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    sortedCalories = sortDesc $ runParser parser input
    p1 = head sortedCalories
    p2 = sum $ take 3 sortedCalories

parser :: Parser [Int]
parser =  parseLinesAndSum `sepBy'` doubleLineEnd
  where
    doubleLineEnd = char '\n' <* char '\n'
    parseLinesAndSum = sum <$> decimal `sepBy'` char '\n'

