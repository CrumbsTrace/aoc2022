module Day6(run) where

import qualified Data.ByteString.Char8 as BS

run :: BS.ByteString -> (Int, Int)
run input = (p1, p2)
  where 
    p1 = startOfPacket (BS.unpack input) 4 0
    p2 = startOfPacket (BS.unpack input) 14 0

startOfPacket :: String -> Int -> Int -> Int
startOfPacket input n offset = let
    characters = take n input
    duplicateIndex = findDuplicateIndex characters 1
  in
    if duplicateIndex == 0 
      then n + offset 
      else startOfPacket (drop duplicateIndex input) n (offset + duplicateIndex)

findDuplicateIndex :: String -> Int -> Int
findDuplicateIndex [] _ = 0
findDuplicateIndex (x:xs) i 
  | x `elem` xs = i
  | otherwise = findDuplicateIndex xs $ i + 1
