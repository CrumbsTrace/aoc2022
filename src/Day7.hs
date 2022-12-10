{-# LANGUAGE OverloadedStrings #-}

module Day7 (run) where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, eitherP, many', string)
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.List (isPrefixOf)
import Utils (parseLine, runParser, skipLine)

type Size = Int

type Name = String

data FileTree = File Size Name | Directory Size Name [FileTree] deriving (Show, Eq)

run :: ByteString -> (Int, Int)
run input = (p1, p2)
  where
    tree = runParser parser input
    sizes = getSizes tree
    p1 = sum $ filter (<= 100_000) sizes
    capacityToFreeUp = 30_000_000 - (70_000_000 - head sizes)
    p2 = minimum $ filter (>= capacityToFreeUp) sizes

getSizes :: FileTree -> [Int]
getSizes (File _ _) = [0]
getSizes (Directory size _ dirs) = size : concatMap getSizes dirs

getSize :: FileTree -> Int
getSize (File size _) = size
getSize (Directory size _ _) = size

parser :: Parser FileTree
parser = parseDir =<< cdParser

parseDir :: String -> Parser FileTree
parseDir dirName = do
  filesAndDirs <- handleCommand =<< parseLine
  let size = sum $ map getSize filesAndDirs
  pure (Directory size dirName filesAndDirs)

handleCommand :: String -> Parser [FileTree]
handleCommand command
  | "$ ls" `isPrefixOf` command = (++) <$> lsParser <*> handleDirs
  | "$ cd " `isPrefixOf` command = handleCd $ drop 5 command
  | otherwise = error "Unrecognized command"

handleCd :: String -> Parser [FileTree]
handleCd ".." = pure []
handleCd dirName = (:) <$> parseDir dirName <*> handleDirs

handleDirs :: Parser [FileTree]
handleDirs = (handleCd =<< cdParser) <|> pure []

cdParser :: Parser String
cdParser = string "$ cd " *> parseLine

lsParser :: Parser [FileTree]
lsParser = rights <$> many' (eitherP dirP fileP)
  where
    dirP = string "dir" *> skipLine
    fileP = File <$> decimal <* char ' ' <*> parseLine
