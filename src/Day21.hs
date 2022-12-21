{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day21 (run) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 as P (Parser, anyChar, char, decimal, many', take)
import Data.ByteString (ByteString)
import Data.Map qualified as M
import Debug.Trace
import Utils (runParser)

data Expression = Value Int | Formula ByteString Char ByteString | Humn [Int -> Int]

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    monkeys = runParser parser input
    p1 = solveP1 monkeys
    p2 = solveP2 monkeys

solveP2 :: M.Map ByteString Expression -> Int
solveP2 monkeys =
  let Formula left _ right = monkeys M.! "root"
      leftResult = go $ monkeys M.! left
      rightResult = go $ monkeys M.! right
   in case (leftResult, rightResult) of
        (Value x, Humn os) -> foldl (\acc f -> f acc) x os
        (Humn os, Value x) -> foldl (\acc f -> f acc) x os
  where
    go x@(Value _) = x
    go (Formula left o right)
      | left == "humn" = Humn [reverseEvaluate left o right $ extractValue $ go $ monkeys M.! right]
      | right == "humn" = Humn [reverseEvaluate left o right $ extractValue $ go $ monkeys M.! left]
      | otherwise =
          let leftResult = go $ monkeys M.! left
              rightResult = go $ monkeys M.! right
           in case (leftResult, rightResult) of
                (Value x, Value y) -> Value $ evaluate o x y
                (Humn os, Value y) -> Humn $ reverseEvaluate "humn" o right y : os
                (Value x, Humn os) -> Humn $ reverseEvaluate left o "humn" x : os

reverseEvaluate :: ByteString -> Char -> ByteString -> Int -> (Int -> Int)
reverseEvaluate _ '*' _ val = (`div` val)
reverseEvaluate _ '+' _ val = (val `subtract`)
reverseEvaluate "humn" '-' _ val = (+ val)
reverseEvaluate _ '-' "humn" val = \x -> -1 * x + val
reverseEvaluate "humn" '/' _ val = (* val)
reverseEvaluate _ '/' "humn" val = \x -> 1 `div` (x `div` val)
reverseEvaluate left _ right val = traceShow (left, right, val) error "aaaa"

extractValue :: Expression -> Int
extractValue (Value x) = x
extractValue _ = error "Not a value"

solveP1 :: M.Map ByteString Expression -> Int
solveP1 monkeys = go $ monkeys M.! "root"
  where
    go (Value x) = x
    go (Formula left o right) =
      let leftResult = go $ monkeys M.! left
          rightResult = go $ monkeys M.! right
       in evaluate o leftResult rightResult

evaluate :: Char -> Int -> Int -> Int
evaluate '*' = (*)
evaluate '-' = (-)
evaluate '/' = div
evaluate _ = (+)

parser :: Parser (M.Map ByteString Expression)
parser = M.fromList <$> many' (equationP <* char '\n')

equationP :: Parser (ByteString, Expression)
equationP = do
  key <- P.take 4 <* P.take 2
  equation <- value <|> formula
  pure (key, equation)
  where
    value = Value <$> decimal
    formula = Formula <$> P.take 4 <*> operator <*> P.take 4
      where
        operator = char ' ' *> anyChar <* char ' '
