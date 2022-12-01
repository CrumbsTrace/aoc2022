import Data.Either
import Days qualified
import Test.Tasty
import Test.Tasty.HUnit
import Utils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Day 1 2021" $ do runAndConfirm (runHelper "Day_1_2021") (1154, 1127),
      testCase "Day 1" $ do runAndConfirm (runHelper "Day_1") (68467, 203420)
    ]

runAndConfirm :: IO (Integer, Integer) -> (Integer, Integer) -> Assertion
runAndConfirm r e = do
  result <- r
  result @?= e

runHelper :: String -> IO (Integer, Integer)
runHelper s = Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
