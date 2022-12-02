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
getPoints (c1, c2) 
  | c1 == c2               = c2 + 3 --Draw
  | c1 - c2 `elem` [-1, 2] = c2 + 6 --Win
  | otherwise              = c2 -- Loss

chooseMove :: (Int, Int) -> (Int, Int)
chooseMove (c1, c2)
  | c2 == 2   = (c1, c1) -- Draw
  | c2 == 3   = (c1, makeValid $ c1 - 2) -- Win
  | otherwise = (c1, makeValid $ c1 - 1) -- Loss

makeValid :: Int -> Int
makeValid c = if c < 1 then c + 3 else c

-- Turn into integer pairs. A and X are converted 1, B, Y to 2 and C, Z to 3
parser :: Parser [(Int, Int)]
parser = (convert <$> anyChar <* space <*> anyChar) `sepBy'` char '\n'
  where 
    convert c1 c2 = (ord c1 - ord '@', ord c2 - ord 'W')
