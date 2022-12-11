{-# LANGUAGE OverloadedStrings #-}

module Day11 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, anyChar, decimal, eitherP, many', sepBy', skipSpace, space, string)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Utils

data Monkey = Monkey
  { items :: [Integer],
    operation :: Integer -> Integer,
    inspect :: Map.Map Int Monkey -> Integer -> Map.Map Int Monkey,
    inspectionCount :: Int,
    factor :: Integer
  }

instance Show Monkey where
  show monkey = show (items monkey)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    monkeys = runParser parser input
    p1 = monkeyBusiness $ monkeyAround monkeys 0 20 True
    p2 = monkeyBusiness $ monkeyAround monkeys 0 10_000 False

monkeyBusiness :: Map.Map Int Monkey -> Int
monkeyBusiness monkeys = product $ take 2 $ sortDesc $ map inspectionCount $ Map.elems monkeys

monkeyAround :: Map.Map Int Monkey -> Int -> Int -> Bool -> Map.Map Int Monkey
monkeyAround monkeys i maxI reduceWorry
  | i == maxI = monkeys
  | otherwise = monkeyAround monkeys' (i + 1) maxI reduceWorry
  where
    monkeys' = foldl (handleMonkey reduceWorry) monkeys [0 .. (length monkeys - 1)]

handleMonkey :: Bool -> Map.Map Int Monkey -> Int -> Map.Map Int Monkey
handleMonkey reduceWorry monkeys index =
  let monkey = monkeys Map.! index
      monkeyItems = items monkey
      monkeys' = foldl (\m s -> inspect monkey m $ uncrazy monkeys $ operation monkey s) monkeys monkeyItems
   in Map.adjust (\m -> m {items = [], inspectionCount = inspectionCount m + length monkeyItems}) index monkeys'
  where
    uncrazy m item = item `mod` product (map factor $ Map.elems m) `div` if reduceWorry then 3 else 1

parser :: Parser (Map.Map Int Monkey)
parser = listToMap <$> many' (parseMonkey <* skipSpace)

parseMonkey :: Parser Monkey
parseMonkey = do
  _ <- skipLine <* skipSpace
  items <- parseItems
  op <- parseOperation
  (factor, inspect) <- parseInspect
  pure $
    Monkey
      { items = items,
        operation = op,
        inspect = inspect,
        inspectionCount = 0,
        factor = factor
      }

parseItems :: Parser [Integer]
parseItems = string "Starting items: " *> (decimal `sepBy'` string ", ") <* skipSpace

parseOperation :: Parser (Integer -> Integer)
parseOperation = do
  _ <- string "Operation: new = old "
  operator <- anyChar
  _ <- space
  variable <- eitherP decimal (string "old")
  _ <- skipSpace
  pure (createOperation operator variable)

createOperation :: Char -> Either Integer ByteString -> (Integer -> Integer)
createOperation '*' (Left n) = (* n)
createOperation '+' (Left n) = (+ n)
createOperation '*' (Right _) = (^ (2 :: Integer))
createOperation _ _ = \n -> n + n

parseInspect :: Parser (Integer, Map.Map Int Monkey -> Integer -> Map.Map Int Monkey)
parseInspect = do
  n <- string "Test: divisible by " *> decimal <* skipSpace
  x <- string "If true: throw to monkey " *> decimal <* skipSpace
  y <- string "If false: throw to monkey " *> decimal <* skipSpace
  pure
    ( n,
      \monkeys i ->
        if i `mod` n == 0
          then addItem monkeys i x
          else addItem monkeys i y
    )

addItem :: Map.Map Int Monkey -> Integer -> Int -> Map.Map Int Monkey
addItem monkeys n index = Map.adjust (\m -> m {items = items m ++ [n]}) index monkeys
