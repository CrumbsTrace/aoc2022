{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day10 (run) where

import Data.Attoparsec.ByteString.Char8 as P (Parser, char, decimal, isSpace, many', signed, takeWhile)
import Data.ByteString (ByteString)
import Utils

data Instruction = Instruction
  { command :: ByteString,
    params :: [Int],
    duration :: Int
  }
  deriving (Show, Eq)

data State = State
  { register :: Int,
    currentCycle :: Int,
    nextExecutionCycle :: Int,
    instructions :: [Instruction]
  }
  deriving (Show, Eq)

type RegisterHistory = [Int]

run :: ByteString -> (Int, [String])
run input = (p1, p2)
  where
    instructions = runParser parser input
    state =
      State
        { register = 1,
          currentCycle = 0,
          nextExecutionCycle = 0,
          instructions = instructions
        }
    registerHistory = runInstructions $ prepareCommand state
    p1 = sum $ getSignalStrengths registerHistory
    p2 = drawSprite registerHistory

runInstructions :: State -> RegisterHistory
runInstructions state@State {instructions, register}
  | null instructions = []
  | otherwise = register : runInstructions (runCycle state)

runCycle :: State -> State
runCycle state@State {instructions, currentCycle, nextExecutionCycle}
  | currentCycle == nextExecutionCycle = (goToNextInstruction . incrementCycle) $ execute current state
  | otherwise = incrementCycle state
  where
    current = head instructions
    goToNextInstruction s = prepareCommand s {instructions = tail instructions}
    incrementCycle s = s {currentCycle = currentCycle + 1}

prepareCommand :: State -> State
prepareCommand state@State {instructions, currentCycle} =
  let nextInstruction = head instructions
   in state {nextExecutionCycle = currentCycle + duration nextInstruction}

execute :: Instruction -> State -> State
execute instruction state
  | command instruction == "addx" = addX instruction state
  | command instruction == "noop" = state
  | otherwise = error "The world is on fire(unrecognized command)"

addX :: Instruction -> State -> State
addX instruction state@State {register} = state {register = register + head (params instruction)}

getSignalStrengths :: RegisterHistory -> [Int]
getSignalStrengths history = (20 * head history') : getStrengthEveryN 60 40 (drop 40 history')
  where
    history' = drop 19 history

getStrengthEveryN :: Int -> Int -> RegisterHistory -> [Int]
getStrengthEveryN _ _ [] = []
getStrengthEveryN i stepSize (x : xs) = i * x : getStrengthEveryN (i + stepSize) stepSize (drop (stepSize - 1) xs)

drawSprite :: RegisterHistory -> [String]
drawSprite [] = []
drawSprite history = drawLine 0 (take 40 history) : drawSprite (drop 40 history)

drawLine :: Int -> RegisterHistory -> String
drawLine _ [] = []
drawLine i (x : xs) = pixel : drawLine (i + 1) xs
  where
    pixel = if abs (x - i) <= 1 then '#' else '.'

parser :: Parser [Instruction]
parser = many' (toInstruction <$> parseCommand <*> parseParams <* char '\n')
  where
    toInstruction command params = Instruction {command = command, params = params, duration = commandDuration command}
    parseCommand = P.takeWhile (not . isSpace)
    parseParams = many' (char ' ' *> signed decimal)
    commandDuration "addx" = 1
    commandDuration _ = 0
