{-# LANGUAGE OverloadedStrings #-}

module Day14 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, char, decimal, many', sepBy', string)
import Data.ByteString (ByteString)
import Data.HashSet qualified as Set
import Utils (runParser)

type Point = (Int, Int)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    cave = runParser parser input
    lowestRock = maximum $ map snd $ Set.toList cave
    p1 = simulateAllSand cave lowestRock True [(500, 0)] 0
    p2 = simulateAllSand cave lowestRock False [(500, 0)] 0

simulateAllSand :: Set.HashSet Point -> Int -> Bool -> [Point] -> Int -> Int
simulateAllSand _ _ _ [] total = total
simulateAllSand occupiedSet lowestRock noFloor previousPath total =
  let path = dropSand occupiedSet (head previousPath) lowestRock noFloor (tail previousPath)
      settledPoint = head path
      occupiedSet' = Set.insert settledPoint occupiedSet
   in if null path then total else simulateAllSand occupiedSet' lowestRock noFloor (tail path) (total + 1)

dropSand :: Set.HashSet Point -> Point -> Int -> Bool -> [Point] -> [Point]
dropSand occupiedSet p@(x, y) lowestRock noFloor path
  | noFloor && y >= lowestRock = []
  | y == lowestRock + 1 = p : path
  | not $ Set.member (x, y + 1) occupiedSet = dropSand occupiedSet (x, y + 1) lowestRock noFloor (p : path)
  | not $ Set.member (x - 1, y + 1) occupiedSet = dropSand occupiedSet (x - 1, y + 1) lowestRock noFloor (p : path)
  | not $ Set.member (x + 1, y + 1) occupiedSet = dropSand occupiedSet (x + 1, y + 1) lowestRock noFloor (p : path)
  | (x, y) == (500, 0) && Set.member (500, 0) occupiedSet = []
  | otherwise = p : path

parser :: Parser (Set.HashSet Point)
parser = Set.fromList . concat <$> many' lineP
  where
    lineP = getRocks <$> (pointP `sepBy'` string " -> ") <* char '\n'
    pointP = (,) <$> decimal <* char ',' <*> decimal
    getRocks ((x, y) : rest@((x2, y2) : _)) = [(x', y') | x' <- getRange x x2, y' <- getRange y y2] ++ getRocks rest
    getRocks _ = []

getRange :: Int -> Int -> [Int]
getRange x y
  | x < y = [x .. y]
  | otherwise = [y .. x]
