module Day25 (run) where

import Data.Attoparsec.ByteString.Char8 (many')
import Data.ByteString (ByteString)
import Utils (parseLine, runParser)

run :: ByteString -> String
run input = p1
  where
    snafus = runParser (many' (reverse <$> parseLine)) input
    p1 = toSnafu 0 $ toBase5 $ sum $ map toNumber snafus

toSnafu :: Int -> [Int] -> String
toSnafu 1 [] = "1"
toSnafu _ [] = ""
toSnafu surplus (n : ns)
  | n + surplus == 5 = toSnafu 1 ns ++ "0"
  | n + surplus == 4 = toSnafu 1 ns ++ "-"
  | n + surplus == 3 = toSnafu 1 ns ++ "="
  | otherwise = toSnafu 0 ns ++ show (n + surplus)

toBase5 :: Int -> [Int]
toBase5 number
  | number <= 5 = [number]
  | otherwise = (number `mod` 5) : toBase5 (number `div` 5)

toNumber :: String -> Int
toNumber snafu = go snafu 1
  where
    go [] _ = 0
    go (n : ns) multiplier = multiplier * asNumber n + go ns (multiplier * 5)

    asNumber c = case c of
      '2' -> 2
      '1' -> 1
      '0' -> 0
      '-' -> -1
      _ -> -2
