module Main (main) where

import qualified System.IO.Strict as S
import qualified Day1

main :: IO ()
main = do 
    day1

day1 :: IO ()
day1 = do runAndConfirm Day1.run "inputs/day1.txt" (1154, 1127)

runAndConfirm :: (String -> (Int, Int)) -> FilePath -> (Int, Int) -> IO ()
runAndConfirm f p r = do
    result <- f <$> S.readFile p
    case result == r of
        True -> return () 
        False -> fail ("Expected " <> show r <> ", Got " <> show result)

