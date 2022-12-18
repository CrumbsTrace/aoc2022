{-# LANGUAGE OverloadedStrings #-}

module Day11 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, anyChar, decimal, eitherP, many', sepBy', skipSpace, space, string)
import Data.ByteString (ByteString)
import Data.List
import Data.Map.Strict qualified as Map
import Utils (runParser, skipLine, sortDesc)

data Monkey = Monkey
  { items :: [Int],
    operation :: Int -> Int,
    inspect :: Map.Map Int Monkey -> Int -> Map.Map Int Monkey,
    inspectCount :: Int,
    factor :: Int
  }

instance Show Monkey where
  show monkey = show (items monkey)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    monkeys = runParser parser input
    p1 = monkeyAround monkeys 20 True
    p2 = monkeyAround monkeys 10_000 False

monkeyBusiness :: Map.Map Int Monkey -> Int
monkeyBusiness monkeys = product $ take 2 $ sortDesc $ map inspectCount $ Map.elems monkeys

monkeyAround :: Map.Map Int Monkey -> Int -> Bool -> Int
monkeyAround monkeys maxI reduceWorry = go monkeys 0
  where
    combinedFactor = product (map factor $ Map.elems monkeys)
    monkeyCount = length monkeys - 1
    go m i
      | i == maxI = monkeyBusiness m
      | otherwise = go m' (i + 1)
      where
        m' = foldl' (handleMonkey reduceWorry combinedFactor) m [0 .. monkeyCount]

handleMonkey :: Bool -> Int -> Map.Map Int Monkey -> Int -> Map.Map Int Monkey
handleMonkey reduceWorry factor monkeys index =
  let monkey = monkeys Map.! index
      monkeyItems = items monkey
      unWorry item = item `mod` factor `div` if reduceWorry then 3 else 1
      clearMonkeyHands = Map.adjust (\m -> m {items = [], inspectCount = inspectCount m + length monkeyItems})
      doInspections = foldl' (\m s -> inspect monkey m $ unWorry $ operation monkey s) monkeys
   in clearMonkeyHands index $ doInspections monkeyItems

parser :: Parser (Map.Map Int Monkey)
parser = listToMap <$> many' (parseMonkey <* skipSpace)
  where
    listToMap xs = Map.fromList $ zip [0 .. length xs - 1] xs

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
        inspectCount = 0,
        factor = factor
      }

parseItems :: Parser [Int]
parseItems = string "Starting items: " *> (decimal `sepBy'` string ", ") <* skipSpace

parseOperation :: Parser (Int -> Int)
parseOperation = do
  _ <- string "Operation: new = old "
  operator <- anyChar
  _ <- space
  variable <- eitherP decimal (string "old")
  _ <- skipSpace
  pure (createOperation operator variable)

createOperation :: Char -> Either Int ByteString -> (Int -> Int)
createOperation '*' (Left n) = (* n)
createOperation '+' (Left n) = (+ n)
createOperation '*' (Right _) = (^ (2 :: Integer))
createOperation _ _ = \n -> n + n

parseInspect :: Parser (Int, Map.Map Int Monkey -> Int -> Map.Map Int Monkey)
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

addItem :: Map.Map Int Monkey -> Int -> Int -> Map.Map Int Monkey
addItem monkeys n index = Map.adjust (\m -> m {items = n : items m}) index monkeys
