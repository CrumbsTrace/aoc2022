{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Day9(run) where

import Utils
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser, string, decimal, many', char, anyChar, notChar)
import qualified Data.Set as Set

type Positions = ((Int, Int), (Int, Int))

run input = (p1, p2)
  where 
    instructions = runParser parser input
    results = foldr moveAndSaveLocations (((0,0), (0,0)), Set.empty) instructions
    p1 = length $ snd results
    p2 = 0

moveAndSaveLocations :: (Int, Int) -> (Positions, Set.Set (Int, Int)) -> (Positions, Set.Set (Int, Int))
moveAndSaveLocations (0, 0) results = results 
moveAndSaveLocations movement (positions, visited) = moveAndSaveLocations newMovement (newPositions, newVisited)
  where 
    newVisited = Set.insert t visited
    (newMovement, newPositions@(_, t)) = move movement positions



move :: (Int, Int) -> Positions -> ((Int, Int), Positions)
move movement (h, t) = (updatedMovement, updatedPositions)
  where 
    updatedPositions = updatePositions (h, t) movement
    updatedMovement = updateMovement movement

updatePositions :: Positions -> (Int, Int) -> Positions
updatePositions (h@(hx, hy), t@(tx, ty)) movement 
  | abs ((hx - tx) + (hy - ty)) <= 1 = (takeStep h movement, t)
  | otherwise = (takeStep h movement, h)

takeStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
takeStep (px, py) (dx, dy) 
  | dx /= 0 = (px + (dx `div` abs dx), py)
  | otherwise = (px, py + (dy `div` abs dy))

updateMovement :: (Int, Int) -> (Int, Int)
updateMovement (dx, dy) 
  | dx /= 0 = ((dx - (dx `div` abs dx)), dy)
  | otherwise = (dx , dy - (dy `div` abs dy))

parser :: Parser [(Int, Int)]
parser = many' (parseInstruction <$> anyChar <* char ' ' <*> decimal <* char '\n')
  where
    parseInstruction 'U' n = (0, n)
    parseInstruction 'D' n = (0, -n)
    parseInstruction 'L' n = (-n, 0)
    parseInstruction r n = (n, 0)
