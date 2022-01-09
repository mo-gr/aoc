module Y2021.AOC2 where

import AOC (Solution (PureSolution))
import Data.Functor (($>))
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (many1, newline, string, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Direction = Up Int | Down Int | Forward Int deriving (Show, Eq)

data Position = Position
  { depth :: Int,
    horizontal :: Int,
    aim :: Int
  }
  deriving (Show, Eq)

startingPos :: Position
startingPos = Position 0 0 0

directionParser :: Parser Direction
directionParser = do
  dir <-
    string "forward " $> Forward
      <|> string "down " $> Down
      <|> string "up " $> Up
  dist <- number <* newline
  pure $ dir dist

inputParser :: Parser [Direction]
inputParser = many1 directionParser

result :: Position -> Int
result (Position d h _a) = d * h

eval :: Position -> [Direction] -> Position
eval = foldl step
  where
    step (Position d h a) (Up n) = Position (d - n) h a
    step (Position d h a) (Down n) = Position (d + n) h a
    step (Position d h a) (Forward n) = Position d (h + n) a

eval' :: Position -> [Direction] -> Position
eval' = foldl step
  where
    step (Position d h a) (Up n) = Position d h (a - n)
    step (Position d h a) (Down n) = Position d h (a + n)
    step (Position d h a) (Forward n) = Position (d + (a * n)) (h + n) a

-- 1815044
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> eval startingPos
    |> result

-- 1739283308
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> eval' startingPos
    |> result

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 1815044 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 1739283308 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 solution2 verify
