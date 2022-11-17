module Day1(run) where

import Utils
import qualified Data.Text as T

run :: T.Text -> (Int, Int)
run input = (p1, p2)
    where 
        numbers = parseNumbers input
        p1 = countIncreases 1 numbers
        p2 = countIncreases 3 numbers
            
countIncreases ::  Int -> [Int] -> Int
countIncreases offset list = length . filter greaterThan $ offsetPairs
    where 
        offsetPairs = getOffsetPairs offset list
        greaterThan = \(x, y) -> y > x

getOffsetPairs :: Int -> [Int] -> [(Int, Int)]
getOffsetPairs offset list = zip list $ drop offset list

