module Utils (readFromFile, runParser) where

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import Data.ByteString qualified as BS

readFromFile :: FilePath -> IO BS.ByteString
readFromFile = BS.readFile

runParser :: Parser [a] -> BS.ByteString -> [a]
runParser p b = case parseOnly p b of
  Left _ -> []
  Right s -> s
