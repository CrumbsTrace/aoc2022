{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day9 (run) where

import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, decimal, many', notChar, string)
import Data.ByteString (ByteString)
import Data.Set qualified as Set
import Debug.Trace
import Utils

type Positions = ((Int, Int), [(Int, Int)])

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    moves = runParser parser input
    p1 = length $ snd $ foldl doMoves (((0, 0), [(0, 0)]), Set.empty) moves
    knots = replicate 9 (0, 0)
    p2 = length $ snd $ foldl doMoves (((0, 0), knots), Set.empty) moves

doMoves :: (Positions, Set.Set (Int, Int)) -> (Int, Int) -> (Positions, Set.Set (Int, Int))
doMoves results (0, 0) = results
doMoves (positions, visited) movement = doMoves (newPositions, newVisited) newMovement
  where
    (newMovement, newPositions@(_, t)) = move movement positions
    newVisited = Set.insert (last t) visited

move :: (Int, Int) -> Positions -> ((Int, Int), Positions)
move movement (h, t) = (newMovement, (newH, newTail))
  where
    newH = takeStep h movement
    newTail = updateTail newH t
    newMovement = updateMovement movement

updateTail :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
updateTail _ [] = []
updateTail p@(px, py) (t@(tx, ty) : ts)
  | abs (px - tx) <= 1 && abs (py - ty) <= 1 = t : ts
  | otherwise = newPos : updateTail newPos ts
  where
    newPos = follow p t

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (px, py) (tx, ty) = (tx + getDir px tx, ty + getDir py ty)

getDir :: Int -> Int -> Int
getDir x y
  | x == y = 0
  | otherwise = unit (x - y)

takeStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
takeStep (px, py) (dx, dy)
  | dx /= 0 = (px + unit dx, py)
  | dy /= 0 = (px, py + unit dy)
  | otherwise = (px, py)

updateMovement :: (Int, Int) -> (Int, Int)
updateMovement (dx, dy)
  | dx /= 0 = (dx - unit dx, dy)
  | otherwise = (dx, dy - unit dy)

unit :: Int -> Int
unit x = x `div` abs x

parser :: Parser [(Int, Int)]
parser = many' (parse <$> anyChar <* char ' ' <*> decimal <* char '\n')
  where
    parse 'U' n = (0, n)
    parse 'D' n = (0, -n)
    parse 'L' n = (-n, 0)
    parse r n = (n, 0)
