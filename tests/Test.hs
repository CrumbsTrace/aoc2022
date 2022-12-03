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
    [ testCase "Day 1 2021" $ do runAndConfirm (runHelper "Day_1_2021") (1154,1127),
      testCase "Day 1" $ do runAndConfirm (runHelper "Day_1") (68467,203420),
      testCase "Day 2" $ do runAndConfirm (runHelper "Day_2") (11906, 11186),
      testCase "Day 3" $ do runAndConfirm (runHelper "Day_3") (7785, 2633)
    ]

runAndConfirm :: Show a => IO String -> a -> Assertion
runAndConfirm r e = do
  result <- r
  result @?= show e

runHelper :: String -> IO String
runHelper s = Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
