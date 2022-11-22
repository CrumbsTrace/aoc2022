module Day1_2021 (run) where

import Data.ByteString qualified as BS
import Utils ( parseNumbers )

run :: BS.ByteString -> (Int, Int)
run input = (p1, p2)
  where
    numbers = parseNumbers input
    p1 = countIncreases 1 numbers
    p2 = countIncreases 3 numbers

countIncreases :: Int -> [Int] -> Int
countIncreases offset xs = sum $ zipWith isIncreaseAsInt xs offsetList
  where
    isIncreaseAsInt x y = fromEnum (x < y)
    offsetList = drop offset xs
