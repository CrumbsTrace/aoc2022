module Day2(run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, char, sepBy', anyChar, space)
import Data.ByteString (ByteString)
import Utils (runParser)
import Data.Char ( ord )

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    parsedInput = runParser parser input
    p1 = sum $ map getPoints parsedInput
    p2 = sum $ map (getPoints . chooseMove) parsedInput

getPoints :: (Int, Int) -> Int
getPoints (x, y) 
  | x == y = y + 3 --Draw
  | x - y `elem` [-1, 2] = y + 6 --Win
  | otherwise = y -- Loss

chooseMove :: (Int, Int) -> (Int, Int)
chooseMove (x, y)
  | y == 2 = (x, x) -- Draw
  | y == 3 = (x, makeValid $ x - 2) -- Win
  | otherwise = (x, makeValid $ x - 1) -- Loss

makeValid :: Int -> Int
makeValid x = if x < 1 then x + 3 else x

-- Turn into integer pairs. A and X are converted 1, B, Y to 2 and C, Z to 3
parser :: Parser [(Int, Int)]
parser = (convert <$> anyChar <* space <*> anyChar) `sepBy'` char '\n'
  where 
    convert x y = (ord x - ord '@', ord y - ord 'W')
