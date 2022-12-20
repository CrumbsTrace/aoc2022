module Day20 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, char, decimal, many', signed)
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Vector.Unboxed ((!))
import Data.Vector.Unboxed qualified as V
import Utils (runParser)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    file = runParser parser input
    numbers = (V.indexed . V.fromList) file
    p1 = mix numbers 1
    p2 = mix (V.map (second (* 811589153)) numbers) 10

mix :: V.Vector (Int, Int) -> Int -> Int
mix numbers times
  | times == 0 = coordinates numbers
  | otherwise = mix (go numbers 0) $ times - 1
  where
    len = V.length numbers - 1
    go ns expectedIndex
      | expectedIndex > len = ns
      | otherwise = go newV (expectedIndex + 1)
      where
        idx = fromJust $ V.findIndex ((== expectedIndex) . fst) ns
        toMove = ns ! idx
        newIdx = mod (idx + snd toMove) len
        (ls, rs) = second V.tail $ V.splitAt idx ns
        newV =
          if newIdx < idx
            then insertBetween toMove (V.splitAt newIdx ls) rs
            else insertBetween toMove (V.splitAt (newIdx - idx) rs) ls

insertBetween :: V.Unbox a => a -> (V.Vector a, V.Vector a) -> V.Vector a -> V.Vector a
insertBetween v (left, right) rest = rest V.++ V.snoc left v V.++ right

coordinates :: V.Vector (Int, Int) -> Int
coordinates result = sum $ map snd [result V.! idx1, result V.! idx2, result V.! idx3]
  where
    zeroIdx = fromJust $ V.findIndex ((== 0) . snd) result
    idx1 = (1000 + zeroIdx) `mod` listLength
    idx2 = (2000 + zeroIdx) `mod` listLength
    idx3 = (3000 + zeroIdx) `mod` listLength
    listLength = V.length result

parser :: Parser [Int]
parser = many' $ signed decimal <* char '\n'
