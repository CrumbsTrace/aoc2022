module Day4(run) where

import Utils (runParser, inBounds)
import Data.Attoparsec.ByteString.Char8 as P (Parser, decimal, many', take)
import Data.ByteString.Char8 as BS (ByteString)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where 
    parsedInput = runParser inputParser input
    p1 = length $ filter (uncurry fullOverlap) parsedInput
    p2 = length $ filter (uncurry overlap) parsedInput

fullOverlap :: (Int, Int) -> (Int, Int) -> Bool
fullOverlap (x1,x2) (x3,x4) = (x1 <= x3 && x2 >= x4) || (x3 <= x1 && x4 >= x2)

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap r1@(x1,x2) r2@(x3,x4) = inBounds x1 r2 || inBounds x2 r2 || inBounds x3 r1 || inBounds x4 r1

-- | A parser for a single range in the input.
rangeParser :: Parser (Int, Int)
rangeParser = do
  a <- decimal  
  _ <- P.take 1
  b <- decimal  
  pure (a, b)  

-- | A parser for a line in the input.
lineParser :: Parser ((Int, Int), (Int, Int))
lineParser = (,) <$> rangeParser <*> (P.take 1 *> rangeParser)  

-- | A parser for the input.
inputParser :: Parser [((Int, Int), (Int, Int))]
inputParser = many' $ lineParser <* P.take 1 
