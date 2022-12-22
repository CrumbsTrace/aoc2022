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
    neighbors,
    add2D,
    combinations,
  )
where

import Data.Attoparsec.ByteString.Char8 as P (Parser, char, many1', notChar, parseOnly, skipWhile)
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
listToMap xs = Map.fromList $ zip [0 .. length xs - 1] xs

listToMap1 :: [b] -> Map.Map Int b
listToMap1 xs = Map.fromList $ zip [1 .. length xs] xs

gridToMap :: [[a]] -> Map.Map (Int, Int) a
gridToMap grid =
  let coordinates = [(row, col) | row <- [0 .. length grid - 1], col <- [0 .. length (head grid) - 1]]
   in Map.fromList (zip coordinates $ concat grid)

outOfBounds :: (Int, Int) -> (Int, Int) -> Bool
outOfBounds (x, y) (width, height)
  | x >= width || x < 0 = True
  | y >= height || y < 0 = True
  | otherwise = False

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (px, py) = [(px - 1, py), (px + 1, py), (px, py + 1), (px, py - 1)]

add2D :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2D (px, py) (dx, dy) = (px + dx, py + dy)

skipLine :: Parser ()
skipLine = skipWhile ('\n' /=) <* char '\n'

parseLine :: Parser [Char]
parseLine = many1' (notChar '\n') <* char '\n'

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y : ys)
  | n < 0 = []
  | otherwise = case drop (n - 1) xs of
      [] -> []
      [_] -> [xs]
      _ ->
        [y : c | c <- combinations (n - 1) ys]
          ++ combinations n ys
