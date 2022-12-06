module Day6(run) where

import Data.List (nub)
import qualified Data.ByteString.Char8 as BS

run :: BS.ByteString -> (Int, Int)
run input = (findStart (BS.unpack input) 4 0, findStart (BS.unpack input) 14 0)
  where
    findStart s n offset 
      | n == (length $ nub $ take n s) = n + offset
      | otherwise = findStart (drop 1 s) n (offset + 1)

