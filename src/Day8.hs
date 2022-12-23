{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day8(run) where

import Utils ( runParser, gridToMap, outOfBounds, add2D )
import Data.Attoparsec.ByteString.Char8 (Parser, many', char, notChar)
import Data.ByteString (ByteString)
import Data.Char (ord)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict ((!))

type Point = (Int, Int) 
type Bounds = (Int, Int) 
type Grid = Map.HashMap Point Int

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where 
    parsedInput = runParser parser input
    bounds = (length $ head parsedInput, length parsedInput)
    grid = gridToMap parsedInput
    results = map (getScoreAndVisibility bounds grid) $ Map.keys grid
    p1 = length $ filter fst results
    p2 = maximum $ map snd results

getScoreAndVisibility :: Bounds -> Grid -> Point -> (Bool, Int)
getScoreAndVisibility bounds grid point = (visible, scenicScore) 
  where
    scoreAndVisibility = countVisibleAllDirs bounds grid point
    visible = any fst scoreAndVisibility
    scenicScore = product $ map snd scoreAndVisibility

countVisibleAllDirs :: Bounds -> Grid -> Point -> [(Bool, Int)]
countVisibleAllDirs bounds grid point = let
  height = grid ! point
  neighbors = [(0, -1),(0, 1),(-1, 0),(1, 0)]
  in map (countVisible point height bounds 0 grid) neighbors

countVisible :: Point -> Int -> Bounds -> Int -> Grid -> Point -> (Bool, Int)
countVisible point pHeight bounds total grid direction
  | outOfBounds nextPoint bounds = (True, total)
  | grid ! nextPoint >= pHeight = (False, total + 1)
  | otherwise = countVisible nextPoint pHeight bounds (total + 1) grid direction
      where nextPoint = add2D point direction

parser :: Parser [[Int]]
parser = many' $ many' (toInt <$> notChar '\n') <* char '\n'
  where 
    toInt c = ord c - 48
