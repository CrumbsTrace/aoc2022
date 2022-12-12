module Utils
  ( readFromFile,
    runParser,
    sortDesc,
    splitInHalf,
    inBounds,
    listToMap1,
    listToMap,
    skipLine,
    parseLine,
    gridToMap,
    outOfBounds,
    add2D,
  )
where

import Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString qualified as BS
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down))

readFromFile :: FilePath -> IO BS.ByteString
readFromFile = BS.readFile

runParser :: Parser a -> BS.ByteString -> a
runParser p b = case parseOnly p b of
  Left _ -> error "Failed to parse input"
  Right s -> s

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortOn Down

splitInHalf :: [Int] -> ([Int], [Int])
splitInHalf s = splitAt (length s `div` 2) s

inBounds :: Int -> (Int, Int) -> Bool
inBounds c (b1, b2) = b1 <= c && b2 >= c

listToMap :: [b] -> Map.Map Int b
listToMap = listToMapHelper Map.empty 0

listToMap1 :: [b] -> Map.Map Int b
listToMap1 = listToMapHelper Map.empty 1

listToMapHelper :: Map.Map Int b -> Int -> [b] -> Map.Map Int b
listToMapHelper stacks _ [] = stacks
listToMapHelper stacks i (x : xs) = listToMapHelper (Map.insert i x stacks) (i + 1) xs

gridToMap :: [[a]] -> Map.Map (Int, Int) a
gridToMap grid =
  let coordinates = [(row, col) | row <- [0 .. length grid - 1], col <- [0 .. length (head grid) - 1]]
   in Map.fromList (zip coordinates $ concat grid)

outOfBounds :: (Int, Int) -> (Int, Int) -> Bool
outOfBounds (x, y) (width, height)
  | x >= width || x < 0 = True
  | y >= height || y < 0 = True
  | otherwise = False

add2D :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2D (px, py) (dx, dy) = (px + dx, py + dy)

skipLine :: Parser ()
skipLine = skipWhile ('\n' /=) <* char '\n'

parseLine :: Parser [Char]
parseLine = many' (notChar '\n') <* char '\n'
