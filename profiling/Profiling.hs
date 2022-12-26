module Main where

import Days (runDay)

main :: IO ()
main = do
  result <- run "Day_23"
  print result

run :: String -> IO String
run s = Days.runDay s $ constructFileName s

constructFileName :: String -> FilePath
constructFileName s = "inputs/" <> s <> ".txt"
