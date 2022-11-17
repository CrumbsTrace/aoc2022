module Day1(run) where

run :: String -> (Int, Int)
run input = (r1, r2)
    where 
        numbers = parse input
        r1 = countIncreases 1 numbers
        r2 = countIncreases 3 numbers

parse :: String -> [Int]
parse input = map (read::String->Int) $ lines input

countIncreases ::  Int -> [Int] -> Int
countIncreases offset list = length $ filter greaterThan offsetPairs
    where 
        offsetPairs = getOffsetPairs offset list
        greaterThan = \(x, y) -> y > x

getOffsetPairs :: Int -> [Int] -> [(Int, Int)]
getOffsetPairs offset list = zip list $ drop offset list

