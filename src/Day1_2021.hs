module Day1_2021 (run) where

import Data.Attoparsec.ByteString.Char8 (Parser, decimal, many')
import Data.Attoparsec.ByteString.Char8 qualified as P (take)
import Data.ByteString qualified as BS
import Utils (runParser)

run :: BS.ByteString -> (Int, Int)
run input = (p1, p2)
  where
    numbers = runParser numberParser input
    p1 = countIncreases 1 numbers
    p2 = countIncreases 3 numbers

countIncreases :: Int -> [Int] -> Int
countIncreases offset list = length [pair | pair@(x, y) <- pairWithOffset offset list, y > x]

pairWithOffset :: Int -> [b] -> [(b, b)]
pairWithOffset o = zip <*> drop o

numberParser :: Parser [Int]
numberParser = many' $ decimal <* P.take 1

