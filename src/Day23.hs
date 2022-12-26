module Day23 (run) where

import Control.Foldl qualified as F
import Data.Attoparsec.ByteString.Char8 (many')
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as M
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.Maybe (fromJust, mapMaybe)
import Utils (gridToMap, parseLine, runParser)

type Point = (Int, Int)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    elves = M.keysSet $ M.filter (== '#') $ gridToMap $ runParser (many' parseLine) input
    p1 = solve elves 10 [0 .. 3]
    p2 = abs (solve elves (-1) [0 .. 3])

solve :: HashSet Point -> Int -> [Int] -> Int
solve elves 0 _ = (maxX - minX + 1) * (maxY - minY + 1) - S.size elves
  where
    (minX, maxX) = bimap fromJust fromJust $ F.fold ((,) <$> F.minimum <*> F.maximum) $ S.map fst elves
    (minY, maxY) = bimap fromJust fromJust $ F.fold ((,) <$> F.minimum <*> F.maximum) $ S.map snd elves
solve elves n options
  | M.null toMove = n
  | otherwise = solve elves' (n - 1) (map (\i -> (i + 1) `mod` 4) options)
  where
    elves' = M.foldlWithKey' (\acc to from -> S.delete from $ S.insert to acc) elves toMove
    toMove = S.foldl' add M.empty elves

    add m p@(x, y)
      | null adj || null moves = m
      | M.member location m = M.delete location m
      | otherwise = M.insert location p m
      where
        adj = filter (`S.member` elves) [(x + 1, y - 1), (x, y + 1), (x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y - 1), (x + 1, y), (x + 1, y + 1)]
        moves = mapMaybe moveCheck options
        location = head moves
        moveCheck i = case i of
          0 -> if all (\(_, y') -> y' /= y - 1) adj then Just (x, y - 1) else Nothing
          1 -> if all (\(_, y') -> y' /= y + 1) adj then Just (x, y + 1) else Nothing
          2 -> if all (\(x', _) -> x' /= x - 1) adj then Just (x - 1, y) else Nothing
          _ -> if all (\(x', _) -> x' /= x + 1) adj then Just (x + 1, y) else Nothing
