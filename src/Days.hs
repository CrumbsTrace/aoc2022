module Days (runDay) where

import Data.ByteString qualified as BS
import Day1 (run)
import Day10 (run)
import Day11 (run)
import Day12 (run)
import Day13 (run)
import Day14 (run)
import Day15 (run)
import Day16 (run)
import Day17 (run)
import Day18 (run)
import Day19 (run)
import Day1_2021 (run)
import Day2 (run)
import Day20 (run)
import Day21 (run)
import Day22 (run)
import Day23 (run)
import Day24 (run)
import Day25 (run)
import Day3 (run)
import Day4 (run)
import Day5 (run)
import Day6 (run)
import Day7 (run)
import Day8 (run)
import Day9 (run)
import Utils (readFromFile)

runDay :: String -> FilePath -> IO String
runDay s p = case s of
  "Day_1" -> runDayF Day1.run p
  "Day_2" -> runDayF Day2.run p
  "Day_3" -> runDayF Day3.run p
  "Day_4" -> runDayF Day4.run p
  "Day_5" -> runDayF Day5.run p
  "Day_6" -> runDayF Day6.run p
  "Day_7" -> runDayF Day7.run p
  "Day_8" -> runDayF Day8.run p
  "Day_9" -> runDayF Day9.run p
  "Day_10" -> runDayF Day10.run p
  "Day_11" -> runDayF Day11.run p
  "Day_12" -> runDayF Day12.run p
  "Day_13" -> runDayF Day13.run p
  "Day_14" -> runDayF Day14.run p
  "Day_15" -> runDayF Day15.run p
  "Day_16" -> runDayF Day16.run p
  "Day_17" -> runDayF Day17.run p
  "Day_18" -> runDayF Day18.run p
  "Day_19" -> runDayF Day19.run p
  "Day_20" -> runDayF Day20.run p
  "Day_21" -> runDayF Day21.run p
  "Day_22" -> runDayF Day22.run p
  "Day_23" -> runDayF Day23.run p
  "Day_24" -> runDayF Day24.run p
  "Day_25" -> runDayF Day25.run p
  "Day_1_2021" -> runDayF Day1_2021.run p
  _ -> return "Error"

runDayF :: Show a => (BS.ByteString -> a) -> FilePath -> IO String
runDayF f p = show . f <$> readFromFile p

