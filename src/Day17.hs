module Day17 (run) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Utils (parseLine, runParser)

type Point = (Int, Int)

type Slice = (S.Set Point, (Int, Int))

type Rock = [Point]

data Tower = Tower
  { towerBlocks :: S.Set Point,
    height :: Int
  }

rocks :: Int -> Rock
rocks 0 = [(0, 0), (1, 0), (2, 0), (3, 0)]
rocks 1 = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
rocks 2 = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
rocks 3 = [(0, 0), (0, 1), (0, 2), (0, 3)]
rocks _ = [(0, 0), (1, 0), (0, 1), (1, 1)]

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    movementPattern = runParser parseLine input
    movements = cycle movementPattern
    p1 = solve 2022 movements
    p2 = solve 1000000000000 movements

solve :: Int -> [Char] -> Int
solve n ms = dropRocks 0 n ms (Tower S.empty 0) M.empty

dropRocks :: Int -> Int -> [Char] -> Tower -> M.Map (S.Set Point) (Int, Int) -> Int
dropRocks i maxI movements tower slices
  | i == maxI = height tower
  | i `mod` 200 == 0 && matchingSlice /= (0, 0) = completeTower maxI matchingSlice s movements
  | otherwise = dropRocks (i + 1) maxI movements' newTower newSlices
  where
    matchingSlice = case M.lookup slice slices of
      Nothing -> (0, 0)
      Just (i2, h2) -> if h2 == height tower then (0, 0) else (i2, h2)

    s@(slice, values) = (copySlice tower, (i, height tower))
    newSlices = if i `mod` 50 == 0 then M.insert slice values slices else slices
    rock = map (\(x, y) -> (x + 2, y + height tower + 4)) $ rocks (i `mod` 5)
    (movements', newTower) = placeRock rock movements tower

completeTower :: Int -> (Int, Int) -> Slice -> [Char] -> Int
completeTower maxI (i2, h2) (set, (i, h)) ms = dropRocks newI maxI ms newTower M.empty
  where
    dI = (maxI - i) `div` (i - i2)
    newHeight = h + dI * (h - h2)
    newTower = Tower newSlice newHeight
    newSlice = S.map (\(x, y) -> (x, y + newHeight - 20)) set
    newI = i + (i - i2) * dI

placeRock :: Rock -> [Char] -> Tower -> ([Char], Tower)
placeRock rock (m : ms) tower
  | success = placeRock movedRock ms tower
  | otherwise = (ms, Tower newBlocks newHeight)
  where
    (success, movedRock) = move rock m tower
    newHeight = max (height tower) (maximum (map snd movedRock))
    newBlocks = foldr S.insert (towerBlocks tower) movedRock

copySlice :: Tower -> S.Set Point
copySlice tower = S.map (\(x, y) -> (x, y - minY)) $ S.filter filterHeights blocks
  where
    filterHeights (_, y) = y > minY && y <= height tower
    blocks = towerBlocks tower
    minY = height tower - 20

move :: Rock -> Char -> Tower -> (Bool, [Point])
move blocks movement Tower {towerBlocks} =
  let deltaX = case movement of
        '<' -> -1
        _ -> 1

      newBlockPoints = map (\(px, py) -> (px + deltaX, py)) blocks
      newBlockPoints' =
        if all (\p@(px, _) -> p `S.notMember` towerBlocks && px >= 0 && px < 7) newBlockPoints
          then newBlockPoints
          else blocks

      newBlockPointsDown = map (\(px, py) -> (px, py - 1)) newBlockPoints'
   in if all (\p@(_, py) -> p `S.notMember` towerBlocks && py > 0) newBlockPointsDown
        then (True, newBlockPointsDown)
        else (False, newBlockPoints')
