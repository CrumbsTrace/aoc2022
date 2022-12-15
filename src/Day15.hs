{-# LANGUAGE OverloadedStrings #-}

module Day15 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, char, decimal, many', signed, string)
import Data.ByteString (ByteString)
import Data.List (nub, sortOn)
import Data.Maybe (fromJust, isJust)
import Utils (runParser)

type Point = (Int, Int)

data Sensor = Sensor
  { position :: Point,
    manhattan :: Int,
    beacon :: Point
  }
  deriving (Show, Eq)

data Range = Range
  { minB :: Int,
    maxB :: Int,
    up :: Bool
  }

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    sensors = runParser parser input
    ranges = mergeRanges $ sortOn minB $ scannedRanges sensors 2_000_000 []
    beaconsOnRow = length $ nub $ filter (\x -> snd x == 2_000_000) $ map beacon sensors
    p1 = foldl (\acc Range {minB, maxB} -> acc + (maxB - minB + 1)) 0 ranges - beaconsOnRow
    p2 = tuningFrequency $ findBeacon sensors 0

tuningFrequency :: Point -> Int
tuningFrequency (x, y) = x * 4_000_000 + y

findBeacon :: [Sensor] -> Int -> Point
findBeacon sensors y
  | isJust beaconX = (fromJust beaconX, y)
  | otherwise = findBeacon sensors $ y + dy
  where
    (x, dy) = getDy $ sortOn minB $ scannedRanges sensors y []
    beaconX = if dy == 0 then Just x else Nothing

getDy :: [Range] -> (Int, Int)
getDy (r1 : rest@(r2 : xs))
  | maxB r1 >= maxB r2 = getDy (r1 : xs)
  | maxB r1 >= minB r2 = (x, min dy calculatedDy)
  | maxB r1 < minB r2 = (maxB r1 + 1, 0)
  where
    (x, dy) = getDy rest
    calculatedDy =
      if up r1 /= up r2
        then min ((maxB r1 - minB r1) `div` 2) ((maxB r2 - minB r2) `div` 2)
        else (maxB r1 - minB r2) `div` 2 + 1
getDy _ = (0, maxBound)

mergeRanges :: [Range] -> [Range]
mergeRanges (r1 : rest@(r2 : xs))
  | maxB r1 >= minB r2 = mergeRanges (Range {minB = minB r1, maxB = max (maxB r1) (maxB r2), up = False} : xs)
  | otherwise = r1 : mergeRanges rest
mergeRanges xs = xs

scannedRanges :: [Sensor] -> Int -> [Range] -> [Range]
scannedRanges [] _ empty = empty
scannedRanges (sensor : xs) y empty
  | distanceToY >= manhattan sensor = scannedRanges xs y empty
  | otherwise = scannedRanges xs y newEmpty
  where
    newEmpty = emptyRange sensor y : empty
    distanceToY = abs (snd (position sensor) - y)

emptyRange :: Sensor -> Int -> Range
emptyRange sensor y = do
  let distanceToY = snd (position sensor) - y
  let margin = manhattan sensor - abs distanceToY
  let xSensor = fst (position sensor)
  Range {minB = xSensor - margin, maxB = xSensor + margin, up = distanceToY > 0}

parser :: Parser [Sensor]
parser = many' $ beaconP <* char '\n'

beaconP :: Parser Sensor
beaconP = do
  x <- string "Sensor at x=" *> signed decimal
  y <- string ", y=" *> signed decimal
  bx <- string ": closest beacon is at x=" *> signed decimal
  by <- string ", y=" *> signed decimal
  pure Sensor {position = (x, y), beacon = (bx, by), manhattan = manhattanDistance (x, y) (bx, by)}

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x, y) (x2, y2) = abs (x2 - x) + abs (y2 - y)
