module Main (main) where

import qualified Day1

main :: IO ()
main = do 
    day1

day1 :: IO ()
day1 = do runAndConfirm Day1.run (1154, 1127)

runAndConfirm :: (IO (Int, Int)) -> (Int, Int) -> IO ()
runAndConfirm f r = do
    result <- f
    case result == r of
        True -> return () 
        False -> fail ("Expected " <> show r <> ", Got " <> show result)

