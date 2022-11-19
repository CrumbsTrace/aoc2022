module Days where

import Utils
import Day1
import Day1_2021
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25


runDay :: String -> FilePath -> IO (Integer, Integer)
runDay s p = case s of
  "Day 1" -> intToInteger <$> Day1.run <$> readFromFile p
  "Day 2" -> intToInteger <$> Day2.run <$> readFromFile p
  "Day 3" -> intToInteger <$> Day3.run <$> readFromFile p
  "Day 4" -> intToInteger <$> Day4.run <$> readFromFile p
  "Day 5" -> intToInteger <$> Day5.run <$> readFromFile p
  "Day 6" -> intToInteger <$> Day6.run <$> readFromFile p
  "Day 7" -> intToInteger <$> Day7.run <$> readFromFile p
  "Day 8" -> intToInteger <$> Day8.run <$> readFromFile p
  "Day 9" -> intToInteger <$> Day9.run <$> readFromFile p
  "Day 10" -> intToInteger <$> Day10.run <$> readFromFile p
  "Day 11" -> intToInteger <$> Day11.run <$> readFromFile p
  "Day 12" -> intToInteger <$> Day12.run <$> readFromFile p
  "Day 13" -> intToInteger <$> Day13.run <$> readFromFile p
  "Day 14" -> intToInteger <$> Day14.run <$> readFromFile p
  "Day 15" -> intToInteger <$> Day15.run <$> readFromFile p
  "Day 16" -> intToInteger <$> Day16.run <$> readFromFile p
  "Day 17" -> intToInteger <$> Day17.run <$> readFromFile p
  "Day 18" -> intToInteger <$> Day18.run <$> readFromFile p
  "Day 19" -> intToInteger <$> Day19.run <$> readFromFile p
  "Day 20" -> intToInteger <$> Day20.run <$> readFromFile p
  "Day 21" -> intToInteger <$> Day21.run <$> readFromFile p
  "Day 22" -> intToInteger <$> Day22.run <$> readFromFile p
  "Day 23" -> intToInteger <$> Day23.run <$> readFromFile p
  "Day 24" -> intToInteger <$> Day24.run <$> readFromFile p
  "Day 25" -> intToInteger <$> Day25.run <$> readFromFile p
  "Day 1 2021" -> intToInteger <$> Day1_2021.run <$> readFromFile p
  _ -> return (0, 0)

intToInteger :: (Int, Int) -> (Integer, Integer)
intToInteger (a, b) = (fromIntegral a, fromIntegral b)
