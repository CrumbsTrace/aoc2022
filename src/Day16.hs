{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day16 (run) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as P (Parser, decimal, many', sepBy', skipSpace, string, take)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Utils (runParser)

data RouteOption = RouteOption
  { valveName :: ByteString,
    dTotal :: Int,
    total :: Int,
    timeRemaining :: Int,
    opened :: Set.Set ByteString
  }
  deriving (Eq, Show)

data Valve = Valve
  { name :: ByteString,
    flowRate :: Int,
    tunnels :: [ByteString]
  }
  deriving (Show, Eq, Ord)

run input = p1
  where
    valves = runParser parser input
    valvesWithFlow = map name $ filter (\v -> flowRate v > 0) $ Map.elems valves
    startingRoute = RouteOption {valveName = "AA", dTotal = 0, total = 0, timeRemaining = 30, opened = Set.empty}
    routesMap = Map.insert "AA" [startingRoute] (Map.fromList $ map (,[]) valvesWithFlow)
    p1 = findBest valves [startingRoute] routesMap valvesWithFlow

findBest :: Map.Map ByteString Valve -> [RouteOption] -> Map.Map ByteString [RouteOption] -> [ByteString] -> Int
findBest valves [] routeOptions _ = maximum $ map (maximum . map (\r -> total r + timeRemaining r * dTotal r)) $ Map.elems routeOptions
findBest valves (route@RouteOption {..} : xs) routeOptions valvesWithFlow
  | valveName /= "AA" && route `notElem` (routeOptions Map.! valveName) = findBest valves xs routeOptions valvesWithFlow
  | otherwise = do
      let valveOptions = mapMaybe (create route valves) $ filter (`Set.notMember` opened) valvesWithFlow
      let newRouteOptions = foldl updateRoutes routeOptions valveOptions
      findBest valves (xs ++ valveOptions) newRouteOptions valvesWithFlow

updateRoutes :: Map.Map ByteString [RouteOption] -> RouteOption -> Map.Map ByteString [RouteOption]
updateRoutes routes route = Map.adjust (filterRoutes route) (valveName route) routes

filterRoutes :: RouteOption -> [RouteOption] -> [RouteOption]
filterRoutes route routes =
  let filteredRoutes = filter (not . worse) routes
   in route : filteredRoutes
  where
    worse thisRoute = dTotal thisRoute <= dTotal route && total thisRoute <= total route && timeRemaining thisRoute <= timeRemaining route

create :: RouteOption -> Map.Map ByteString Valve -> ByteString -> Maybe RouteOption
create route valves goal = do
  let distance = 1 + bfs valves goal Set.empty [(0, valveName route)]
  if timeRemaining route - distance <= 0
    then Nothing
    else
      Just
        RouteOption
          { valveName = goal,
            opened = Set.insert goal $ opened route,
            total = total route + distance * dTotal route,
            dTotal = dTotal route + flowRate (valves Map.! goal),
            timeRemaining = timeRemaining route - distance
          }

-- | Used to find the fastest path to a different valve to open
bfs :: Map.Map ByteString Valve -> ByteString -> Set.Set ByteString -> [(Int, ByteString)] -> Int
bfs _ _ _ [] = error "Destination could not be reached"
bfs valves end visited ((distance, valveName) : toVisit)
  | valveName == end = distance
  | otherwise =
      let newPoints = filter (`Set.notMember` visited) (tunnels $ valves Map.! valveName)
          newVisited = foldr Set.insert visited newPoints
          newToVisit = toVisit ++ map (distance + 1,) newPoints
       in bfs valves end newVisited newToVisit

parser :: Parser (Map.Map ByteString Valve)
parser = Map.fromList <$> many' (valveP <* skipSpace)

valveP :: Parser (ByteString, Valve)
valveP = do
  name <- string "Valve " *> P.take 2
  flowRate <- string " has flow rate=" *> decimal
  tunnels <-
    ((: []) <$> (string "; tunnel leads to valve " *> P.take 2))
      <|> (string "; tunnels lead to valves " *> P.take 2 `sepBy'` string ", ")
  pure (name, Valve {name = name, flowRate = flowRate, tunnels = tunnels})
