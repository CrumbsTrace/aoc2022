module Day23 (run) where

import Control.Foldl qualified as F
import Data.Attoparsec.ByteString.Char8 (many')
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as M
import Data.IntSet (IntSet)
import Data.IntSet qualified as S
import Data.Maybe (fromJust, mapMaybe)
import Utils (gridToMap, parseLine, runParser)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    elves = S.fromList $ map pointToInt $ M.keys $ M.filter (== '#') $ gridToMap $ runParser (many' parseLine) input
    p1 = solve elves 10 [0 .. 3]
    p2 = abs (solve elves (-1) [0 .. 3])

solve :: IntSet -> Int -> [Int] -> Int
solve elves 0 _ = (maxX - minX + 1) * (maxY - minY + 1) - S.size elves
  where
    elfPoints = map intToPoint $ S.elems elves
    (minX, maxX) = bimap fromJust fromJust $ F.fold ((,) <$> F.minimum <*> F.maximum) $ map fst elfPoints
    (minY, maxY) = bimap fromJust fromJust $ F.fold ((,) <$> F.minimum <*> F.maximum) $ map snd elfPoints
solve elves n options
  | M.null toMove = n
  | otherwise = solve elves' (n - 1) (map (\i -> (i + 1) `mod` 4) options)
  where
    elves' = M.foldrWithKey' (\to from acc -> S.delete from $ S.insert to acc) elves toMove
    toMove = S.foldl' add M.empty elves

    add m p
      | null adj || null moves = m
      | M.member location m = M.delete location m
      | otherwise = M.insert location p m
      where
        adj = filter (`S.member` elves) [p - 1, p - 5001, p + 4999, p + 5000, p - 5000, p + 1, p - 4999, p + 5001]

        moves = mapMaybe moveCheck options
        location = head moves
        moveCheck i = case i of
          0 -> if all (\p' -> p' /= p - 5000 && p' /= p - 5001 && p' /= p - 4999) adj then Just (p - 5000) else Nothing
          1 -> if all (\p' -> p' /= p + 5000 && p' /= p + 5001 && p' /= p + 4999) adj then Just (p + 5000) else Nothing
          2 -> if all (\p' -> p' /= p - 1 && p' /= p - 5001 && p' /= p + 4999) adj then Just (p - 1) else Nothing
          _ -> if all (\p' -> p' /= p + 1 && p' /= p + 5001 && p' /= p - 4999) adj then Just (p + 1) else Nothing

intToPoint :: Int -> (Int, Int)
intToPoint n = (x, y)
  where
    y = (n - x) `div` 5000
    x = n `rem` 5000

pointToInt :: (Int, Int) -> Int
pointToInt (x, y) = (y + 2500) * 5000 + (x + 2500)
