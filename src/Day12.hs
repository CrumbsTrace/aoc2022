module Day12 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, many')
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Utils (gridToMap, neighbors, parseLine, runParser)

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
bfs' grid end starts = bfs grid end (Set.fromList starts) (map (0,) starts)

bfs :: Grid -> Point -> Set.Set Point -> [(Int, Point)] -> Int
bfs _ _ _ [] = maxBound -- We ran out of places to visit
bfs grid end visited ((distance, pos) : toVisit)
  | pos == end = distance
  | otherwise = do
      let newPoints = filter traversable $ neighbors pos
      let newVisited = foldr Set.insert visited newPoints
      let newToVisit = toVisit ++ map (distance + 1,) newPoints
      bfs grid end newVisited newToVisit
  where
    currentHeight = height grid pos
    traversable point =
      height grid point - currentHeight <= 1
        && point `Set.notMember` visited

height :: Grid -> Point -> Int
height grid point = case Map.lookup point grid of
  Just 'S' -> ord 'a'
  Just 'E' -> ord 'z'
  Just c -> ord c
  Nothing -> maxBound -- Out of bounds

parser :: Parser Grid
parser = gridToMap <$> many' parseLine
