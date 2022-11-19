import qualified Data.Text as T
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
  [ testCase "Day 1 2021" $ do runAndConfirm (Days.runDay "Day 1 2021" "inputs/Day_1_2021.txt") (1154, 1127)
  ]

runAndConfirm :: IO (Integer, Integer) -> (Integer, Integer) -> Assertion
runAndConfirm r e = do
    result <- r
    result @?= e 

-- testParsing :: Assertion
-- testParsing = 
--     assertBool "Failed to parse cuboid" $ isRight $ parseCuboid "off x=10..19,y=8..12,z=10..12"
