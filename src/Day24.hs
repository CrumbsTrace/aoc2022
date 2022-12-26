module Day24 (run) where

import Data.Attoparsec.ByteString.Char8 (many')
import Data.ByteString (ByteString)
import Data.HashSet qualified as S
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Utils (parseLine, runParser)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    grid = runParser (V.fromList <$> many' (V.fromList <$> parseLine)) input
    (p1, p2) = navigate grid

navigate :: Vector (Vector Char) -> (Int, Int)
navigate grid =
  let firstRun = startRun begin end 0
      withSnacks = startRun begin end $ startRun end begin firstRun
   in (firstRun, withSnacks)
  where
    startRun start = go (S.singleton start)
    begin = (0, -1)
    end = (w - 1, h)

    w = V.length (grid ! 0) - 2
    h = V.length grid - 2
    xs = V.fromList [0 .. w - 1]
    ys = V.fromList [0 .. h - 1]
    -- Blizzards
    ups = V.map (\x -> V.map (\y -> get (x, y) /= '^') ys) xs
    downs = V.map (\x -> V.map (\y -> get (x, y) /= 'v') ys) xs
    lefts = V.map (\y -> V.map (\x -> get (x, y) /= '<') xs) ys
    rights = V.map (\y -> V.map (\x -> get (x, y) /= '>') xs) ys

    go points goal t
      | S.member goal points = t
      | otherwise = go newPoints goal t'
      where
        t' = t + 1
        newPoints = foldr (\p acc -> foldr S.insert acc (getNewPoints p)) S.empty points
        getNewPoints pos@(x, y) =
          filter
            (\p' -> p' == end || p' == begin || isSafe t' p')
            [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1), pos]

    get (x, y) = (grid ! (y + 1)) ! (x + 1)
    inBounds (x, y) = x >= 0 && x < w && y >= 0 && y < h
    isSafe t p@(x, y) =
      inBounds p
        && (lefts ! y) ! ((x + t) `mod` w)
        && (rights ! y) ! ((x - t) `mod` w)
        && (ups ! x) ! ((y + t) `mod` h)
        && (downs ! x) ! ((y - t) `mod` h)
