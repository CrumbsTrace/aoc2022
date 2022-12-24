module Day24 (run) where

import Data.Attoparsec.ByteString.Char8 (many')
import Data.ByteString (ByteString)
import Data.HashSet qualified as S
import Data.Maybe (fromJust)
import Data.PQueue.Prio.Min qualified as P
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Utils (parseLine, runParser)

type Point = (Int, Int)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    grid = runParser (V.fromList <$> many' (V.fromList <$> parseLine)) input
    p1 = navigate grid False
    p2 = navigate grid True

navigate :: Vector (Vector Char) -> Bool -> Int
navigate grid getSnacks
  | getSnacks = startRun begin end $ startRun end begin $ startRun begin end 0
  | otherwise = startRun begin end 0
  where
    startRun start goal elapsed = go (P.singleton (manhattan start goal) (elapsed, start)) S.empty goal
    begin = (1, 0)
    end = (w, h + 1)

    w = V.length (grid ! 0) - 2
    h = V.length grid - 2
    xs = V.fromList [0 .. w - 1]
    ys = V.fromList [0 .. h - 1]
    -- Blizzards
    ups = V.map (\x -> V.map (\y -> get (x, y) /= '^') ys) xs
    downs = V.map (\x -> V.map (\y -> get (x, y) /= 'v') ys) xs
    lefts = V.map (\y -> V.map (\x -> get (x, y) /= '<') xs) ys
    rights = V.map (\y -> V.map (\x -> get (x, y) /= '>') xs) ys

    go queue visited goal
      | pos == goal = t
      | (pos, t) `S.member` visited = go queue' visited goal
      | otherwise = go newQueue (S.insert (pos, t) visited) goal
      where
        ((t, pos@(x, y)), queue') = fromJust $ P.minView queue
        moves =
          filter
            (\p' -> p' == end || p' == begin || (isSafe (t + 1) p' && not (S.member (p', t + 1) visited)))
            [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1), (x, y)]
        newQueue = foldr (\p' acc -> P.insert ((t + 1) + manhattan p' goal) (t + 1, p') acc) queue' moves

    get (x, y) = (grid ! (y + 1)) ! (x + 1)
    inBounds (x, y) = x > 0 && x <= w && y > 0 && y <= h
    isSafe t p@(x, y) =
      inBounds p
        && (ups ! (x - 1)) ! (((y + t) - 1) `mod` h)
        && (downs ! (x - 1)) ! (((y - t) - 1) `mod` h)
        && (lefts ! (y - 1)) ! (((x + t) - 1) `mod` w)
        && (rights ! (y - 1)) ! (((x - t) - 1) `mod` w)

manhattan :: Point -> Point -> Int
manhattan (x, y) (x2, y2) = abs (x2 - x) + abs (y2 - y)
