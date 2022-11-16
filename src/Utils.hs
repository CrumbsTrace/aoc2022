module Utils where

getLinesFromFile :: FilePath -> IO [String]
getLinesFromFile p = lines <$> readFile p

getNumbersFromFile :: FilePath -> IO [Int]
getNumbersFromFile p = map read <$> lines <$> readFile p

getCharactersFromFile :: FilePath -> IO [Char]
getCharactersFromFile p = map read <$> lines <$> readFile p

