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
  _ -> return "Error"

runDayInt :: Show a => (BS.ByteString -> a) -> FilePath -> IO String
runDayInt f p = show . f <$> readFromFile p

