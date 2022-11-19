module Day1_2021(run) where

import Utils
import qualified Data.ByteString as BS

run :: BS.ByteString -> (Int, Int)
run input = (p1, p2)
    where 
        numbers = parseNumbers input
        p1 = countIncreases 1 numbers
        p2 = countIncreases 3 numbers

countIncreases ::  Int -> [Int] -> Int
countIncreases offset list = length . filter id $ increases
    where 
        increases = checkIncreases offset list

checkIncreases :: Int -> [Int] -> [Bool]
checkIncreases offset list = zipWith (<) list $ drop offset list

