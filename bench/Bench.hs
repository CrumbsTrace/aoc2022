import Criterion.Main
import Utils
import qualified Data.Text as T
import Days

main = do 
  -- let days = [benchDay ("Day " <> show i) | i <- [1..25]]
  let days = [benchDay "Day 1 2021"]
  defaultMain [
    bgroup "days" days 
    ]


benchDay :: String -> Benchmark
benchDay s = bench s $ nfIO $ Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> map replaceSpaces s <> ".txt"

replaceSpaces :: Char -> Char
replaceSpaces ' ' = '_'
replaceSpaces s = s
