module Utils(readFromFile, parseNumbers) where

import qualified Data.ByteString as BS
-- import qualified Data.Char as C
import Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative

readFromFile :: FilePath -> IO BS.ByteString
readFromFile = BS.readFile

numberParser :: Parser [Int]
numberParser = many $ P.decimal <* P.skipSpace

parseNumbers :: BS.ByteString -> [Int]
parseNumbers b = case P.parseOnly numberParser b of
    Left _ -> []
    Right s -> s


