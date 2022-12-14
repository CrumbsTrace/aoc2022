import Criterion.Main
import Days (runDay)

main = do
  -- let days = [benchDay ("Day_" <> show i) | i <- [1 .. 12]]
  let days = [benchDay "Day_14"]
  defaultMain [bgroup "days" days]

benchDay :: String -> Benchmark
benchDay s = bench s $ nfIO $ Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
