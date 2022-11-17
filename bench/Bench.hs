import Criterion.Main
import Utils
import Day1

main = do 
  day1Input <- readFromFile "inputs/day1.txt"
  defaultMain [
    bgroup "days" [ bench "Day 1" $ nf Day1.run day1Input ]
    ]
