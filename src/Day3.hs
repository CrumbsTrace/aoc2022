module Day3 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, char, many1', notChar, sepBy')
import Data.ByteString.Char8 as BS (ByteString)
import Data.Char (ord)
import Utils (runParser, splitInHalf)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    parsedInput = runParser parser input
    p1 = sum $ map (findCommonPair . splitInHalf) parsedInput
    p2 = sum $ findCommonTrios parsedInput

findCommonPair :: ([Int], [Int]) -> Int
findCommonPair (l1, l2) = head [x | x <- l1, x `elem` l2]

findCommonTrios :: [[Int]] -> [Int]
findCommonTrios (l1 : l2 : l3 : ls) = head [x | x <- l1, x `elem` l2, x `elem` l3] : findCommonTrios ls
findCommonTrios _ = []

parser :: Parser [[Int]]
parser = parseItems `sepBy'` char '\n'
  where
    parseItems = many1' $ evaluate . ord <$> notChar '\n'
    evaluate c = if c > 96 then c - 96 else c - 38
