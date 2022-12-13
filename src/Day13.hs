module Day13 (run) where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.ByteString.Char8 as P (Parser, char, count, decimal, many', sepBy', skipSpace)
import Data.ByteString (ByteString)
import Data.List (findIndices, sort)
import Utils (runParser)

data Packet = Value Int | List [Packet] deriving (Show, Eq)

instance Ord Packet where
  compare (Value x) (Value y) = compare x y
  compare (List x) (List y) = compare x y
  compare x@(Value _) y@(List _) = compare (List [x]) y
  compare x@(List _) y@(Value _) = compare x (List [y])

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    packets = runParser parser input
    p1 = sum $ findIndices' ((==) <*> sort) packets

    keys = [List [List [Value 6]], List [List [Value 2]]]
    p2 = product $ findIndices' (`elem` keys) $ sort $ keys ++ concat packets

findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' f p = map (+ 1) $ findIndices f p

parser :: Parser [[Packet]]
parser = many' (count 2 (packet <* skipSpace))
  where
    packet = value <|> list
    value = Value <$> decimal
    list = List <$> (char '[' *> packet `sepBy'` char ',' <* char ']')
