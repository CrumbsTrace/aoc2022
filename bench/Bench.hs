import Criterion.Main
import Day1
import qualified System.IO.Strict as S

main = do 
  day1Input <- S.readFile "inputs/day1.txt"
  defaultMain [
    bgroup "days" [ bench "Day 1" $ nf Day1.run day1Input ]
    ]
