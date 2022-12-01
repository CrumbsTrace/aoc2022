module Utils (readFromFile, runParser, sortDesc, bstring) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, parseOnly, string)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (pack)
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

bstring :: String -> Parser BS.ByteString
bstring s = P.string (pack s)
