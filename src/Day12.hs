module Day12 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, char, letter_ascii, many')
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as Set
import Data.Vector ((!))
import Data.Vector qualified as V
import Utils (neighbors, outOfBounds, runParser)

type Point = (Int, Int)

type Grid = V.Vector (V.Vector Int)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    grid = runParser parser input
    p1 = bfs grid 69 False $ findStart grid 'S'
    p2 = bfs grid 97 True $ findStart grid 'E'

findStart :: Grid -> Char -> Point
findStart grid c =
  let ord_c = ord c
      y = fromJust $ V.findIndex (isJust . V.findIndex (== ord_c)) grid
      x = fromJust $ V.findIndex (== ord_c) $ grid V.! y
   in (x, y)

bfs :: Grid -> Int -> Bool -> Point -> Int
bfs grid end goUp start = go (Set.singleton start) [(0, start)]
  where
    go _ [] = maxBound -- We ran out of places to visit
    go visited ((distance, pos@(x, y)) : toVisit)
      | (grid ! y) ! x == end = distance
      | otherwise =
          let newPoints = filter traversable $ neighbors pos
              newVisited = foldr Set.insert visited newPoints
              newToVisit = toVisit ++ map (distance + 1,) newPoints
           in go newVisited newToVisit
      where
        currentHeight = height grid pos
        traversable point =
          reachable grid point currentHeight goUp
            && point `Set.notMember` visited

reachable :: Grid -> Point -> Int -> Bool -> Bool
reachable grid point currentHeight goUp
  | outOfBounds point dimensions = False
  | goUp = currentHeight - pointHeight <= 1
  | otherwise = pointHeight - currentHeight <= 1
  where
    pointHeight = height grid point
    dimensions = (V.length (grid ! 0), V.length grid)

height :: Grid -> Point -> Int
height grid (x, y) = case (grid ! y) ! x of
  83 -> 97 -- 'S' = 'a'
  69 -> 122 -- 'E' = 'z'
  c -> c

parser :: Parser Grid
parser = V.fromList <$> many' (V.fromList <$> many' (ord <$> letter_ascii) <* char '\n')
