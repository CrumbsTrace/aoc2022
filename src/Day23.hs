module Day23 (run) where

import Control.Foldl qualified as F
import Data.Attoparsec.ByteString.Char8 (many')
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Utils (gridToMap, parseLine, runParser)

type Point = (Int, Int)

type Moves = Point -> Set Point -> Maybe Point

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    elves = S.fromList $ M.keys $ M.filter (== '#') $ gridToMap $ runParser (many' parseLine) input
    p1 = solve elves 10 priorities
    p2 = abs (solve elves (-1) priorities)
    priorities =
      [ \(x, y) m -> if all (`S.notMember` m) [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)] then Just (x, y - 1) else Nothing,
        \(x, y) m -> if all (`S.notMember` m) [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)] then Just (x, y + 1) else Nothing,
        \(x, y) m -> if all (`S.notMember` m) [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)] then Just (x - 1, y) else Nothing,
        \(x, y) m -> if all (`S.notMember` m) [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)] then Just (x + 1, y) else Nothing
      ]

solve :: Set Point -> Int -> [Moves] -> Int
solve elves 0 _ = length $ [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY], (x, y) `S.notMember` elves]
  where
    (minX, maxX) = bimap fromJust fromJust $ F.fold ((,) <$> F.minimum <*> F.maximum) $ S.map fst elves
    (minY, maxY) = bimap fromJust fromJust $ F.fold ((,) <$> F.minimum <*> F.maximum) $ S.map snd elves
solve elves n prios
  | elves' == elves = n
  | otherwise = solve elves' (n - 1) (tail prios ++ [head prios])
  where
    elves' = go elves prios
    go s p = S.fromList $ move ++ stay
      where
        proposed = S.fold (add p) M.empty s
        move = M.keys $ M.filter ((== 1) . length) proposed
        stay = concat $ M.elems $ M.filter ((> 1) . length) proposed

    add o p@(x, y) = M.alter (Just . maybe [p] (p :)) location
      where
        location = if noNeighbors || null options then p else head options
        noNeighbors = all (`S.notMember` elves) [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]
        options = mapMaybe (\f -> f p elves) o
