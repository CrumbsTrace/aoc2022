module Day1_2021 (run) where

import Control.Applicative (Alternative (many))
import Data.Attoparsec.ByteString.Char8 (Parser, decimal, skipSpace)
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
pairWithOffset o xs = zip xs (drop o xs)

numberParser :: Parser [Int]
numberParser = many $ decimal <* skipSpace
