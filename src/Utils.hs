module Utils (readFromFile, runParser, sortDesc) where

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import Data.ByteString qualified as BS
import Data.List
import Data.Ord (Down (Down))

readFromFile :: FilePath -> IO BS.ByteString
readFromFile = BS.readFile

runParser :: Parser [a] -> BS.ByteString -> [a]
runParser p b = case parseOnly p b of
  Left _ -> []
  Right s -> s

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortOn Down
