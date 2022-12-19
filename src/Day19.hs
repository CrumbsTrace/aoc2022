{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day19 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, decimal, many', string)
import Data.ByteString (ByteString)
import Utils

data Blueprint = Blueprint
  { blueprintId :: Int,
    oreRobot :: Int,
    clayRobot :: Int,
    obsidianRobot :: (Int, Int),
    geodeRobot :: (Int, Int)
  }
  deriving (Show, Eq)

data Plan = Plan
  { oreRobots :: Int,
    clayRobots :: Int,
    obsidianRobots :: Int,
    geodeRobots :: Int,
    oreCount :: Int,
    clayCount :: Int,
    obsidianCount :: Int,
    geodeCount :: Int,
    timeRemaining :: Int,
    didNothing :: Bool,
    couldveDone :: [Bool]
  }
  deriving (Show, Eq)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    blueprints = runParser parser input
    p1 = sum $ map (\b -> blueprintId b * optimalGeodes 24 b) blueprints
    p2 = product $ map (optimalGeodes 32) $ take 3 blueprints

optimalGeodes :: Int -> Blueprint -> Int
optimalGeodes startTime blueprint = go [originalPlan] 0
  where
    originalPlan = Plan 1 0 0 0 0 0 0 0 startTime False []
    go [] bestFound = bestFound
    go (p : ps) bestFound
      | timeRemaining p == 0 = go ps $ max bestFound (geodeCount p)
      | worse = go ps bestFound
      | canBuildGeodeEveryStep = go (newGeodeRobot ps) bestFound
      | otherwise = go ((newGeodeRobot . newObsidianRobot . newOreRobot . newClayRobot) (p' : ps)) bestFound
      where
        canBuildGeodeEveryStep = oreRobots p >= fst (geodeRobot blueprint) && obsidianRobots p >= snd (geodeRobot blueprint)
        canBuildOreRobot = oreCount p >= oreRobot blueprint
        canBuildClayRobot = oreCount p >= clayRobot blueprint
        canBuildObsidianRobot = oreCount p >= fst (obsidianRobot blueprint) && clayCount p >= snd (obsidianRobot blueprint)
        canBuildGeodeRobot = oreCount p >= fst (geodeRobot blueprint) && obsidianCount p >= snd (geodeRobot blueprint)

        p' =
          p
            { oreCount = oreCount p + oreRobots p,
              clayCount = clayCount p + clayRobots p,
              obsidianCount = obsidianCount p + obsidianRobots p,
              geodeCount = geodeCount p + geodeRobots p,
              timeRemaining = timeRemaining p - 1,
              didNothing = True,
              couldveDone = [canBuildOreRobot, canBuildClayRobot, canBuildObsidianRobot, canBuildGeodeRobot]
            }

        worse = geodeCount p + timeRemaining p * (geodeRobots p + (timeRemaining p `div` 2)) <= bestFound

        newOreRobot plans =
          if not (didNothing p && head (couldveDone p)) && clayRobots p == 0 && timeRemaining p >= 4 && canBuildOreRobot
            then p' {oreRobots = oreRobots p' + 1, oreCount = oreCount p' - oreRobot blueprint, didNothing = False} : plans
            else plans
        newClayRobot plans =
          if not (didNothing p && couldveDone p !! 1) && timeRemaining p >= 6 && canBuildClayRobot
            then p' {clayRobots = clayRobots p' + 1, oreCount = oreCount p' - clayRobot blueprint, didNothing = False} : plans
            else plans
        newObsidianRobot plans =
          if not (didNothing p && couldveDone p !! 2) && timeRemaining p >= 4 && canBuildObsidianRobot
            then
              p'
                { obsidianRobots = obsidianRobots p' + 1,
                  oreCount = oreCount p' - fst (obsidianRobot blueprint),
                  clayCount = clayCount p' - snd (obsidianRobot blueprint),
                  didNothing = False
                }
                : plans
            else plans
        newGeodeRobot plans =
          if not (didNothing p && couldveDone p !! 3) && timeRemaining p >= 2 && canBuildGeodeRobot
            then
              p'
                { geodeRobots = geodeRobots p' + 1,
                  oreCount = oreCount p' - fst (geodeRobot blueprint),
                  obsidianCount = obsidianCount p' - snd (geodeRobot blueprint),
                  didNothing = False
                }
                : plans
            else plans

parser :: Parser [Blueprint]
parser = many' blueprintP

blueprintP :: Parser Blueprint
blueprintP = do
  blueprintId <- string "Blueprint " *> decimal
  oreRobot <- string ": Each ore robot costs " *> decimal
  clayRobot <- string " ore. Each clay robot costs " *> decimal
  obsidianRobot <- (,) <$> (string " ore. Each obsidian robot costs " *> decimal) <*> (string " ore and " *> decimal)
  geodeRobot <- (,) <$> (string " clay. Each geode robot costs " *> decimal) <*> (string " ore and " *> decimal)
  _ <- string " obsidian.\n"
  pure $ Blueprint blueprintId oreRobot clayRobot obsidianRobot geodeRobot
