module Day1 (run) where

import Data.Attoparsec.ByteString.Char8 (Parser, decimal, many')
import Data.Attoparsec.ByteString.Char8 qualified as P (take)
import Data.ByteString (ByteString)
import Utils (runParser, sortDesc)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    sortedCalories = sortDesc $ map sum $ runParser parser input
    p1 = head sortedCalories
    p2 = sum $ take 3 sortedCalories

parser :: Parser [[Int]]
parser =  many' $ parseCalorieLines <* P.take 1
  where
    parseCalorieLines = many' $ decimal <* P.take 1

