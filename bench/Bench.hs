import Criterion.Main
import Days
import Utils

main = do
  -- let days = [benchDay ("Day_" <> show i) | i <- [1 .. 6]]
  let days = [benchDay "Day_8"]
  defaultMain [bgroup "days" days]

benchDay :: String -> Benchmark
benchDay s = bench s $ nfIO $ Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
