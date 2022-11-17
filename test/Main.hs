module Main (main) where

import qualified Data.Text as T
import qualified Day1
import Utils

main :: IO ()
main = do 
    runAndConfirm Day1.run "inputs/day1.txt" (1154, 1127)

runAndConfirm :: (T.Text -> (Int, Int)) -> FilePath -> (Int, Int) -> IO ()
runAndConfirm f p r = do
    input <- readFromFile p
    let result = f input
    case result == r of
        True -> return () 
        False -> fail ("Expected " <> show r <> ", Got " <> show result)

-- testParsing :: IO ()
-- testParsing = do
--     case parseCuboid "on x=10..12,y=10..12,z=10..12" of
--         Left a -> fail a
--         Right _ -> return ()


