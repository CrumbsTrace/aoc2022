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
      testCase "Day 3" $ do runAndConfirm (runHelper "Day_3") (7785, 2633),
      testCase "Day 4" $ do runAndConfirm (runHelper "Day_4") (475, 825),
      testCase "Day 5" $ do runAndConfirm (runHelper "Day_5") ("SHQWSRBDL", "CDTQZHBRS"),
      testCase "Day 6" $ do runAndConfirm (runHelper "Day_6") (1804, 2508),
      testCase "Day 7" $ do runAndConfirm (runHelper "Day_7") (1334506, 7421137),
      testCase "Day 8" $ do runAndConfirm (runHelper "Day_8") (1816, 383520),
      testCase "Day 9" $ do runAndConfirm (runHelper "Day_9") (1816, 383520)
    ]

runAndConfirm :: Show a => IO String -> a -> Assertion
runAndConfirm r e = do
  result <- r
  result @?= show e

runHelper :: String -> IO String
runHelper s = Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
