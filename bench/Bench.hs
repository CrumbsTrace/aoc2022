import Criterion.Main
import Days
import Utils

main = do
  let days = [benchDay ("Day_" <> show i) | i <- [2 .. 2]]
  -- let days = [benchDay "Day_1_2021"]
  defaultMain [ bgroup "days" days ]

benchDay :: String -> Benchmark
benchDay s = bench s $ nfIO $ Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
