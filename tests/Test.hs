import qualified Days
import Utils

import Test.Tasty
import Test.Tasty.HUnit
import Data.Either

import Data.List
import Data.Ord

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ 
    testCase "Day 1 2021" $ do runAndConfirm (runHelper "Day_1_2021") (1154, 1127)
  ]

runAndConfirm :: IO (Integer, Integer) -> (Integer, Integer) -> Assertion
runAndConfirm r e = do
    result <- r
    result @?= e 

runHelper :: String -> IO (Integer, Integer)
runHelper s = Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
