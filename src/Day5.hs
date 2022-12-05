{-# LANGUAGE OverloadedStrings #-}
module Day5(run) where

import Utils (runParser, listToMap1, skipLine)
import Data.Attoparsec.ByteString.Char8 (Parser, string, decimal, many', anyChar, sepBy', char)
import qualified Data.Attoparsec.ByteString.Char8 as P (take)
import Data.ByteString (ByteString)
import Data.List (transpose)
import Data.Map.Strict qualified as Map

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
moveCrates atOnce cargo (n, x, y) = let
    toMove = if atOnce
             then take n $ cargo Map.! x
             else (reverse . take n) $ cargo Map.! x
    removeFromStack = Map.adjust (drop n) x
    addToStack = Map.adjust (toMove ++) y
  in
    (addToStack . removeFromStack) cargo

topCrates :: Cargo -> String
topCrates cargo = map head $ Map.elems cargo

parser :: Parser (Cargo, [Instruction])
parser = (,) <$> (convertToCargo <$> many' stackLineP) 
             <*> (skipLine *> many' instructionP)

instructionP :: Parser Instruction
instructionP = (,,) <$> (string "move " *> decimal)
                    <*> (string " from " *> decimal)
                    <*> (string " to " *> decimal)
                    <* P.take 1

stackLineP :: Parser [Char]
stackLineP = (crateP `sepBy'` char ' ') <* char '\n'

crateP :: Parser Char
crateP = P.take 1 *> anyChar <* P.take 1

convertToCargo :: [[Char]] -> Cargo
convertToCargo stacks = (listToMap1 . removeSpaces) $ transpose stacks
  where 
    removeSpaces = map (filter (' '/=))
