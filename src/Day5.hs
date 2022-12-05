{-# LANGUAGE OverloadedStrings #-}
module Day5(run) where

import Utils (runParser, listToMap1)
import Data.Attoparsec.ByteString.Char8 (Parser, string, decimal, many', anyChar, skipWhile, sepBy', char)
import Data.Attoparsec.ByteString.Char8 qualified as P (take)
import Data.ByteString (ByteString)
import Data.List (transpose)
import Data.Map.Strict qualified as Map
import Data.Map.Strict ((!))

type Cargo = Map.Map Int [Char]
type Instruction = (Int, Int, Int)

run :: ByteString -> (String, String)
run input = (p1, p2)
  where 
    (cargo, instructions) = runParser parser input
    p1 = topCrates $ rearrange False cargo instructions
    p2 = topCrates $ rearrange True cargo instructions

rearrange :: Bool -> Cargo -> [Instruction] -> Cargo
rearrange atOnce = foldl (moveCrates atOnce)

moveCrates :: Bool -> Cargo -> Instruction -> Cargo
moveCrates atOnce cargo instruction@(n, x, _) = updateCargo toMove instruction cargo
  where
    toMove = grabCrates n x cargo atOnce

updateCargo :: [Char] -> Instruction -> Cargo -> Cargo
updateCargo toMove (n,x,y) cargo = (addToStack . removeFromStack) cargo
  where 
    addToStack = Map.adjust (toMove ++) y
    removeFromStack = Map.adjust (drop n) x 

grabCrates :: Int -> Int -> Cargo -> Bool -> [Char]
grabCrates n index cargo atOnce
  | atOnce = take n $ cargo!index
  | otherwise = (reverse . take n) $ cargo!index

topCrates :: Cargo -> String
topCrates cargo = map head $ Map.elems cargo

parser :: Parser (Cargo, [Instruction])
parser = do 
  cargo <- convertToCargo <$> many' stackLineP
  _ <- skipWhile ('m'/=)
  instructions <- many' instructionP
  pure (cargo, instructions)

instructionP :: Parser Instruction
instructionP = do 
  x <- string "move " *> decimal
  y <- string " from " *> decimal
  z <- string " to " *> decimal
  _ <- P.take 1
  pure (x, y, z)

stackLineP :: Parser [Char]
stackLineP = (crateP `sepBy'` char ' ') <* char '\n'

crateP :: Parser Char
crateP = P.take 1 *> anyChar <* P.take 1

convertToCargo :: [[Char]] -> Cargo
convertToCargo stacks = (listToMap1 . removeSpaces) $ transpose stacks
  where 
    removeSpaces = map (filter (' '/=))
