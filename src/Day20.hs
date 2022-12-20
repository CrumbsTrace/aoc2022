module Day20 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, decimal, many', signed, skipSpace)
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as V
import Utils (runParser)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    file = runParser parser input
    numbers = (V.indexed . V.fromList) file
    p1 = getCoordinates $ mix numbers 0
    inputPart2 = (V.indexed . V.fromList) $ map (* 811589153) file
    p2 = getCoordinates $ foldl' mix inputPart2 [0 .. 9]

mix :: V.Vector (Int, Int) -> Int -> V.Vector (Int, Int)
mix ns _ = foldl' move ns [0 .. V.length ns - 1]

move :: V.Vector (Int, Int) -> Int -> V.Vector (Int, Int)
move ns expectedIndex
  | newIdx < idx = insertBetween current (V.splitAt splitIndex ls) rs
  | otherwise = insertBetween current (V.splitAt splitIndex rs) ls
  where
    idx = fromJust $ V.findIndex ((== expectedIndex) . fst) ns
    current = ns V.! idx
    newIdx = mod (idx + snd current) (V.length ns - 1)
    (ls, rs) = second V.tail $ V.splitAt idx ns
    splitIndex = if newIdx < idx then newIdx else newIdx - idx

insertBetween :: V.Unbox a => a -> (V.Vector a, V.Vector a) -> V.Vector a -> V.Vector a
insertBetween v p rest = V.snoc (fst p) v V.++ snd p V.++ rest

getCoordinates :: V.Vector (Int, Int) -> Int
getCoordinates result = sum $ map snd [result V.! idx1, result V.! idx2, result V.! idx3]
  where
    zeroIdx = fromJust $ V.findIndex ((== 0) . snd) result
    idx1 = (1000 + zeroIdx) `mod` listLength
    idx2 = (2000 + zeroIdx) `mod` listLength
    idx3 = (3000 + zeroIdx) `mod` listLength
    listLength = V.length result

parser :: Parser [Int]
parser = many' $ signed decimal <* skipSpace
