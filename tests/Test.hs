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
      testCase "Day 1" $ do runAndConfirm (runHelper "Day_1") (68467, 203420),
      testCase "Day 2" $ do runAndConfirm (runHelper "Day_2") (11906, 11186),
      testCase "Day 3" $ do runAndConfirm (runHelper "Day_3") (7785, 2633),
      testCase "Day 4" $ do runAndConfirm (runHelper "Day_4") (475, 825),
      testCase "Day 5" $ do runAndConfirm (runHelper "Day_5") ("SHQWSRBDL", "CDTQZHBRS"),
      testCase "Day 6" $ do runAndConfirm (runHelper "Day_6") (1804, 2508),
      testCase "Day 7" $ do runAndConfirm (runHelper "Day_7") (1334506, 7421137),
      testCase "Day 8" $ do runAndConfirm (runHelper "Day_8") (1816, 383520),
      testCase "Day 9" $ do runAndConfirm (runHelper "Day_9") (6367, 2536),
      testCase "Day 10" $ do runAndConfirm (runHelper "Day_10") (14780, ["####.#....###..#....####..##..####.#....", "#....#....#..#.#.......#.#..#....#.#....", "###..#....#..#.#......#..#......#..#....", "#....#....###..#.....#...#.##..#...#....", "#....#....#....#....#....#..#.#....#....", "####.####.#....####.####..###.####.####."]),
      testCase "Day 11" $ do runAndConfirm (runHelper "Day_11") (56120, 24389045529),
      testCase "Day 12" $ do runAndConfirm (runHelper "Day_12") (447, 446),
      testCase "Day 13" $ do runAndConfirm (runHelper "Day_13") (5659, 22110),
      testCase "Day 14" $ do runAndConfirm (runHelper "Day_14") (763, 23921),
      testCase "Day 15" $ do runAndConfirm (runHelper "Day_15") (5083287, 13134039205729),
      testCase "Day 16" $ do runAndConfirm (runHelper "Day_16") (1701, 2455),
      testCase "Day 17" $ do runAndConfirm (runHelper "Day_17") (3200, 1584927536247),
      testCase "Day 18" $ do runAndConfirm (runHelper "Day_18") (4242, 2428),
      testCase "Day 19" $ do runAndConfirm (runHelper "Day_19") (1365, 4864),
      testCase "Day 20" $ do runAndConfirm (runHelper "Day_20") (7004, 17200008919529),
      testCase "Day 21" $ do runAndConfirm (runHelper "Day_21") (291425799367130, 3219579395609),
      testCase "Day 22" $ do runAndConfirm (runHelper "Day_22") (27436, 15426),
      testCase "Day 23" $ do runAndConfirm (runHelper "Day_23") (3996, 908)
    ]

runAndConfirm :: Show a => IO String -> a -> Assertion
runAndConfirm r e = do
  result <- r
  result @?= show e

runHelper :: String -> IO String
runHelper s = Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
