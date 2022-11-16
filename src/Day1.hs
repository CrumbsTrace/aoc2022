module Day1(run) where

import Utils

run :: IO (Int, Int)
run = do 
    numbers <- getNumbersFromFile "inputs/day1.txt"
    let r1 = countIncreases 1 numbers
    let r2 = countIncreases 3 numbers
    return (r1, r2)

countIncreases ::  Int -> [Int] -> Int
countIncreases offset list = length $ filter greaterThan offsetPairs
    where 
        offsetPairs = getOffsetPairs offset list
        greaterThan = \(x, y) -> y > x

getOffsetPairs :: Int -> [Int] -> [(Int, Int)]
getOffsetPairs offset list = zip list $ drop offset list

