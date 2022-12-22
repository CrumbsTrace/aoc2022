module Day22 (run) where

import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, decimal, many')
import Data.ByteString (ByteString)
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector (Vector, (!), (!?))
import Data.Vector qualified as V
import Debug.Trace
import Utils (parseLine, runParser)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    (board, instructions) = runParser parser input
    p1 = walk board instructions False
    p2 = walk board instructions True

walk :: Vector (Vector Char) -> [(Int, Char)] -> Bool -> Int
walk board instructions p2 = go instructions startPosition (1, 0)
  where
    startPosition = (fromJust (V.findIndex (== '.') $ board ! 0), 0)
    size = minimum $ V.map V.length board
    width = maximum $ V.map V.length board
    height = V.length board
    go [] (x, y) (dx, dy) = 1000 * (y + 1) + 4 * (x + 1) + facingNumber
      where
        facingNumber =
          case (dx, dy) of
            (1, 0) -> 0
            (0, 1) -> 1
            (-1, 0) -> 2
            _ -> 3
    go ((n, i) : is) (x, y) (dx, dy) = go is newPos newDir'
      where
        newDir' = turn i newDir
        (newPos, newDir) = move n ((x, y), (dx, dy))
        wrap (x', y') dir'
          | not p2 = moveToBoard (x' + (x - x') * width, y' + (y - y') * height) dir'
          | y' == -1 && x' < size * 2 = verify ((0, x' + 2 * size), (1, 0)) -- red circle
          | x' == size - 1 && y' < size = verify ((0, (size * 3 - y') - 1), (1, 0)) -- green circle
          | y' == -1 && x' >= 2 * size = verify ((x' - size * 2, 4 * size - 1), (0, -1)) -- orange squircle
          | x' == size * 3 = verify ((size * 2 - 1, (size * 3 - y') - 1), (-1, 0)) -- green squircle
          | y' == size && x' >= size * 2 && y' /= y = verify ((size * 2 - 1, x' - size), (-1, 0)) -- red squircle
          | x' == size * 2 && y' < 2 * size && x' /= x = verify ((y' + size, size - 1), (0, -1)) -- red squircle
          | x' == size - 1 && y' >= size && x' /= x = verify ((y' - size, size * 2), (0, 1)) -- orange circle
          | y' == size * 2 - 1 && x' < size && y' /= y = verify ((size, size + x'), (1, 0)) -- orange circle
          | x' == size * 2 && y' >= size * 2 = verify ((size * 3 - 1, (size * 3 - y') - 1), (-1, 0)) -- green squircle
          | x' == -1 && y' < size * 3 = verify ((size, (size * 3 - y') - 1), (1, 0)) -- green circle
          | y' == size * 3 && x' >= size && y' /= y = verify ((size - 1, x' + size * 2), (-1, 0)) -- blue circle
          | x' == size && y' >= size * 3 && x' /= x = verify ((y' - size * 2, size * 3 - 1), (0, -1)) -- blue circle
          | x' == -1 && y' >= size * 3 = verify ((y' - size * 2, 0), (0, 1)) -- red circle
          | y' == size * 4 && x' < size = verify ((x' + size * 2, 0), (0, 1)) -- orange squircle
          | otherwise = traceShow (x', y') error "AAAAAAAAAA"
          where
            verify r@(p, _) = if getTile board p == Just '.' then Just r else Nothing

        move 0 r = r
        move n' r@((x', y'), dir'@(dx', dy')) = case getTile board newXY of
          Nothing -> move (n' - 1) $ fromMaybe r $ wrap newXY dir'
          Just ' ' -> move (n' - 1) $ fromMaybe r $ wrap newXY dir'
          Just '#' -> r
          Just '.' -> move (n' - 1) (newXY, dir')
          _ -> error "Impossible tile"
          where
            newXY = (x' + dx', y' + dy')

        moveToBoard (x', y') dir' = case getTile board (x', y') of
          Nothing -> moveToBoard (x' + dx, y' + dy) dir'
          Just ' ' -> moveToBoard (x' + dx, y' + dy) dir'
          Just '#' -> Nothing
          Just '.' -> Just ((x', y'), dir')
          _ -> error "Impossible tile"

getTile :: Vector (Vector Char) -> (Int, Int) -> Maybe Char
getTile board (x, y) = case board !? y of
  Nothing -> Nothing
  Just row -> case row !? x of
    Nothing -> Nothing
    c -> c

turn :: Char -> (Int, Int) -> (Int, Int)
turn 'L' d = case d of
  (1, 0) -> (0, -1)
  (0, 1) -> (1, 0)
  (-1, 0) -> (0, 1)
  _ -> (-1, 0)
turn 'R' d = case d of
  (1, 0) -> (0, 1)
  (0, 1) -> (-1, 0)
  (-1, 0) -> (0, -1)
  _ -> (1, 0)
turn _ d = d

parser :: Parser (Vector (Vector Char), [(Int, Char)])
parser =
  (,)
    <$> (V.fromList . map V.fromList <$> many' parseLine)
    <* char '\n'
    <*> many' ((,) <$> decimal <*> anyChar)
