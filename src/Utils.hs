module Utils where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C
import Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Internal.Types as Internal
import Control.Applicative

readFromFile :: FilePath -> IO T.Text
readFromFile = TIO.readFile

numberParser :: Internal.Parser T.Text [Int]
numberParser = many (fromInteger <$> P.decimal <* char '\n')

runByteStringParser :: Parser a -> T.Text -> IResult T.Text a
runByteStringParser p bs = case parse p bs of 
    Partial f -> f T.empty
    v -> v

parseNumbers :: T.Text -> [Int]
parseNumbers b = case runByteStringParser numberParser b of
    Done t r | T.null t -> r
    _ -> []




-- Example of how parsing could be done when I need it
data CoordRange = CoordRange Int Int deriving Show
data OnOff = OnOff Bool deriving Show
data Cuboid = Cuboid OnOff CoordRange CoordRange CoordRange deriving Show

onOff :: Parser OnOff
onOff = do
    state <- (\t -> t == T.pack "on") <$> P.takeWhile1 (not . C.isSpace)
    P.skipWhile (not . C.isDigit)
    return $ OnOff state

coordRange :: Parser CoordRange
coordRange = do
    c1 <- fromInteger <$> decimal
    P.skipWhile (not . C.isDigit)
    c2 <- fromInteger <$> decimal
    P.skipWhile (not . C.isDigit)
    return $ CoordRange c1 c2

cuboid :: Parser Cuboid
cuboid = do
    o <- onOff
    xRange <- coordRange
    yRange <- coordRange
    zRange <- coordRange
    return $ Cuboid o xRange yRange zRange
    
parseCuboid :: String -> Either String Cuboid
parseCuboid s = P.parseOnly cuboid $ T.pack s




