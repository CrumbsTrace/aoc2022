import qualified Data.Text as T
import qualified Day1
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
  [ testCase "Day 1" $ do runAndConfirm Day1.run "inputs/day1.txt" (1154, 1127)
  ]

runAndConfirm :: (T.Text -> (Int, Int)) -> FilePath -> (Int, Int) -> Assertion
runAndConfirm f p r = do
    input <- readFromFile p
    let result = f input
    result @?= r 

-- testParsing :: Assertion
-- testParsing = 
--     assertBool "Failed to parse cuboid" $ isRight $ parseCuboid "off x=10..19,y=8..12,z=10..12"
