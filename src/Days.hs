module Days (runDay) where

import Data.ByteString qualified as BS
import Day1
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
import Day1_2021
import Day2
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Utils

runDay :: String -> FilePath -> IO (Integer, Integer)
runDay s p = case s of
  "Day_1" -> runDayInt Day1.run p
  "Day_2" -> runDayInt Day2.run p
  "Day_3" -> runDayInt Day3.run p
  "Day_4" -> runDayInt Day4.run p
  "Day_5" -> runDayInt Day5.run p
  "Day_6" -> runDayInt Day6.run p
  "Day_7" -> runDayInt Day7.run p
  "Day_8" -> runDayInt Day8.run p
  "Day_9" -> runDayInt Day9.run p
  "Day_10" -> runDayInt Day10.run p
  "Day_11" -> runDayInt Day11.run p
  "Day_12" -> runDayInt Day12.run p
  "Day_13" -> runDayInt Day13.run p
  "Day_14" -> runDayInt Day14.run p
  "Day_15" -> runDayInt Day15.run p
  "Day_16" -> runDayInt Day16.run p
  "Day_17" -> runDayInt Day17.run p
  "Day_18" -> runDayInt Day18.run p
  "Day_19" -> runDayInt Day19.run p
  "Day_20" -> runDayInt Day20.run p
  "Day_21" -> runDayInt Day21.run p
  "Day_22" -> runDayInt Day22.run p
  "Day_23" -> runDayInt Day23.run p
  "Day_24" -> runDayInt Day24.run p
  "Day_25" -> runDayInt Day25.run p
  "Day_1_2021" -> runDayInt Day1_2021.run p
  _ -> return (0, 0)

runDayInt :: (BS.ByteString -> (Int, Int)) -> FilePath -> IO (Integer, Integer)
runDayInt f p = intToInteger . f <$> readFromFile p

intToInteger :: (Int, Int) -> (Integer, Integer)
intToInteger (a, b) = (fromIntegral a, fromIntegral b)
