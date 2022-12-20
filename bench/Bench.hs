import Criterion.Main
import Days (runDay)

main = do
  -- let days = [benchDay ("Day_" <> show i) | i <- [1 .. 18]]
  let days = [bench "All Days" $ nfIO runAllDays]
  -- let days = [benchDay "Day_20"]
  defaultMain [bgroup "days" days]

benchDay :: String -> Benchmark
benchDay s = bench s $ nfIO $ Days.runDay s $ constructFileName s

run :: String -> IO String
run s = Days.runDay s $ constructFileName s

runAllDays :: IO String
runAllDays = do
  day1 <- run "Day_1"
  day2 <- run "Day_2"
  day3 <- run "Day_3"
  day4 <- run "Day_4"
  day5 <- run "Day_5"
  day6 <- run "Day_6"
  day7 <- run "Day_7"
  day8 <- run "Day_8"
  day9 <- run "Day_9"
  day10 <- run "Day_10"
  day11 <- run "Day_11"
  day12 <- run "Day_12"
  day13 <- run "Day_13"
  day14 <- run "Day_14"
  day15 <- run "Day_15"
  day16 <- run "Day_16"
  day17 <- run "Day_17"
  day18 <- run "Day_18"
  day19 <- run "Day_19"
  day20 <- run "Day_20"
  pure
    ( day1
        ++ day2
        ++ day3
        ++ day4
        ++ day5
        ++ day6
        ++ day7
        ++ day8
        ++ day10
        ++ day11
        ++ day12
        ++ day13
        ++ day14
        ++ day15
        ++ day16
        ++ day17
        ++ day18
        ++ day19
        ++ day20
    )

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
