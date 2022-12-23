module Day18 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, char, decimal, many', skipSpace)
import Data.ByteString (ByteString)
import Data.HashSet qualified as S
import Utils (runParser)

type Point = (Int, Int, Int)

run :: ByteString -> (Int, Int)
run input = (length unconnected, countExposed grid unconnected)
  where
    grid = runParser parser input
    unconnected = concatMap (filter (\e -> not (e `S.member` grid)) . neighbors) grid

countExposed :: S.HashSet Point -> [Point] -> Int
countExposed grid points = go points [] 0
  where
    go [] _ total = total
    go (p : ps) regions total = go ps regions' newTotal
      where
        region = filter (S.member p . snd) regions
        ((open, _), regions') =
          if null region
            then ((,) <*> (: regions)) $ floodFill bounds grid p
            else (head region, regions)
        newTotal = if open then total + 1 else total
    findMaxBy f = maximum $ S.map f grid
    bounds = (findMaxBy (\(x, _, _) -> x), findMaxBy (\(_, y, _) -> y), findMaxBy (\(_, _, z) -> z))

floodFill :: Point -> S.HashSet Point -> Point -> (Bool, S.HashSet Point)
floodFill (w, h, d) grid point = go (False, S.singleton point) point
  where
    go (openAir, area) p = foldl go (openAir', area') $ filter inBounds neighborsP
      where
        area' = foldr S.insert area neighborsP
        openAir' = openAir || any inOpenAir neighborsP
        neighborsP = filter (\n -> not (n `S.member` grid) && not (n `S.member` area)) $ neighbors p
        inOpenAir (x, y, z) = x <= 0 || x >= w || y <= 0 || y >= h || z <= 0 || z >= d
        inBounds (x, y, z) = x >= 0 && x <= w && y >= 0 && y <= h && z >= 0 && z <= d

neighbors :: Point -> [Point]
neighbors (x, y, z) =
  [ (x - 1, y, z),
    (x + 1, y, z),
    (x, y - 1, z),
    (x, y + 1, z),
    (x, y, z - 1),
    (x, y, z + 1)
  ]

parser :: Parser (S.HashSet Point)
parser = S.fromList <$> many' (((,,) <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal) <* skipSpace)
