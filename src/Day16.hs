{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day16 (run) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as P (Parser, decimal, many', sepBy', skipSpace, string, take)
import Data.ByteString (ByteString)
import Data.List (tails)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
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

type ValveMap = Map.Map ByteString Valve

type DistanceMap = Map.Map (ByteString, ByteString) Int

data Valve = Valve
  { name :: ByteString,
    flowRate :: Int,
    tunnels :: [ByteString]
  }
  deriving (Show, Eq, Ord)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    valves = runParser parser input
    valvesWithFlow = Set.fromList $ map name $ filter (\v -> flowRate v > 0) $ Map.elems valves
    p1 = maximum $ calculateRoutes valves valvesWithFlow 30
    p2Routes = calculateRoutes valves valvesWithFlow 26
    p2 =
      maximum
        [ v1 + v2
          | (opened1, v1) : rest <- tails (Map.assocs p2Routes),
            (opened2, v2) <- rest,
            not (overlap opened1 opened2)
        ]

overlap :: Set.Set ByteString -> Set.Set ByteString -> Bool
overlap set1 set2 = any (`Set.member` set2) set1

calculateRoutes :: ValveMap -> Set.Set ByteString -> Int -> Map.Map (Set.Set ByteString) Int
calculateRoutes valves valvesWithFlow timeLimit =
  let startingRoute = RouteOption {valveName = "AA", dTotal = 0, total = 0, timeRemaining = timeLimit, opened = Set.empty}
      routesMap = Map.insert "AA" [startingRoute] (Map.fromList $ map (,[]) $ Set.toList valvesWithFlow)
      best = findBest valves [startingRoute] routesMap valvesWithFlow Map.empty
   in Map.fromListWith max $ map (\r -> (opened r, totalPressure r)) best
  where
    totalPressure r = total r + timeRemaining r * dTotal r

findBest :: ValveMap -> [RouteOption] -> Map.Map ByteString [RouteOption] -> Set.Set ByteString -> DistanceMap -> [RouteOption]
findBest _ [] routeOptions _ _ = concat $ Map.elems routeOptions
findBest valves (route@RouteOption {..} : xs) routeOptions valvesWithFlow distanceMap
  | valveName /= "AA" && route `notElem` (routeOptions Map.! valveName) = findBest valves xs routeOptions valvesWithFlow distanceMap
  | otherwise = do
      let toVisit = Set.difference valvesWithFlow opened
      let (distanceMap', valveOptions) = foldl (create route valves) (distanceMap, []) toVisit
      let newRouteOptions = foldl updateRoutes routeOptions valveOptions
      findBest valves (xs ++ valveOptions) newRouteOptions valvesWithFlow distanceMap'

updateRoutes :: Map.Map ByteString [RouteOption] -> RouteOption -> Map.Map ByteString [RouteOption]
updateRoutes routes route = Map.adjust (filterRoutes route) (valveName route) routes

filterRoutes :: RouteOption -> [RouteOption] -> [RouteOption]
filterRoutes route routes =
  let filteredRoutes = filter (not . worse route) routes
   in if any (`worse` route) filteredRoutes then filteredRoutes else route : filteredRoutes
  where
    worse route1 route2 = dTotal route2 <= dTotal route1 && total route2 <= total route1 && timeRemaining route2 <= timeRemaining route1

create :: RouteOption -> Map.Map ByteString Valve -> (DistanceMap, [RouteOption]) -> ByteString -> (DistanceMap, [RouteOption])
create route valves (distanceMap, routeOptions) goal = do
  let maybeDistance = Map.lookup (valveName route, goal) distanceMap
  let distance = fromMaybe (1 + bfs valves goal Set.empty [(0, valveName route)]) maybeDistance
  let distanceMap' =
        if isNothing maybeDistance
          then Map.insert (goal, valveName route) distance $ Map.insert (valveName route, goal) distance distanceMap
          else distanceMap
  if timeRemaining route - distance <= 0
    then (distanceMap', routeOptions)
    else
      ( distanceMap',
        RouteOption
          { valveName = goal,
            opened = Set.insert goal $ opened route,
            total = total route + distance * dTotal route,
            dTotal = dTotal route + flowRate (valves Map.! goal),
            timeRemaining = timeRemaining route - distance
          }
          : routeOptions
      )

-- | Used to find the fastest path to a different valve to open
bfs :: ValveMap -> ByteString -> Set.Set ByteString -> [(Int, ByteString)] -> Int
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
