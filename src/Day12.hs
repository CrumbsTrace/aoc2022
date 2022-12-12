module Day12 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, many')
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Utils (gridToMap, parseLine, runParser)

type Point = (Int, Int)

type Grid = Map.Map Point Char

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    grid = runParser parser input
    end = head . Map.keys $ Map.filter (== 'E') grid
    p1 = bfs' grid end $ Map.keys $ Map.filter (== 'S') grid
    p2 = bfs' grid end $ Map.keys $ Map.filter (== 'a') grid

bfs' :: Grid -> Point -> [Point] -> Int
bfs' grid goal starts = bfs grid goal (Set.fromList starts) (map (0,) starts)

bfs :: Grid -> Point -> Set.Set Point -> [(Int, Point)] -> Int
bfs _ _ _ [] = maxBound -- We ran out of places to visit
bfs grid goal visited ((distance, pos) : toVisit)
  | grid ! pos == 'E' = distance
  | otherwise = do
      let newPoints = filter traversable $ neighbors pos
      let newVisited = foldr Set.insert visited newPoints
      let newToVisit = toVisit ++ map (distance + 1,) newPoints
      bfs grid goal newVisited newToVisit
  where
    traversable point =
      point `Map.member` grid
        && height grid point - height grid pos <= 1
        && point `Set.notMember` visited

neighbors :: Point -> [Point]
neighbors (px, py) = [(px - 1, py), (px + 1, py), (px, py + 1), (px, py - 1)]

height :: Grid -> Point -> Int
height grid point = case grid ! point of
  'S' -> ord 'a'
  'E' -> ord 'z'
  c -> ord c

parser :: Parser Grid
parser = gridToMap <$> many' parseLine
