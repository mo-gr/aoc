module Y2015.AOC3 where

import Data.Either (fromRight)
import Data.List (nub)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (char, many1, (<|>))
import Text.Parsec.ByteString (Parser, parseFromFile)

data Direction = North | South | East | West deriving (Show, Eq)

type Location = (Int, Int)

directionParser :: Parser Direction
directionParser = do
  char '>' *> pure East
    <|> char '<' *> pure West
    <|> char '^' *> pure North
    <|> char 'v' *> pure South

inputParser :: Parser [Direction]
inputParser = many1 directionParser

move :: Location -> Direction -> Location
move (x, y) North = (x, y + 1)
move (x, y) South = (x, y - 1)
move (x, y) East = (x + 1, y)
move (x, y) West = (x - 1, y)

run :: [Direction] -> Location -> [Location]
run ds start = foldl (\acc d -> move (head acc) d : acc) [start] ds

dezip :: [a] -> ([a], [a])
dezip as = dropLast $ foldl f ([], [], 0) $ reverse as
  where
    f :: ([a], [a], Int) -> a -> ([a], [a], Int)
    f (xs, xs', i) x | odd i = (x : xs, xs', i + 1)
    f (xs, xs', i) x = (xs, x : xs', i + 1)
    dropLast :: (a, b, c) -> (a, b)
    dropLast (a, b, _) = (a, b)

-- 2572
solution1 :: IO Int
solution1 = do
  input <- fromRight [] <$> parseFromFile inputParser "AOC3.input"
  pure $ length $ nub $ run input (0, 0)

-- 2631
solution2 :: IO Int
solution2 = do
  input <- fromRight [] <$> parseFromFile inputParser "AOC3.input"
  let (santa, robot) = dezip input
  pure $ length . nub $ run santa (0, 0) ++ run robot (0, 0)

verify :: Test
verify =
  TestList
    [ TestCase (solution1 >>= assertEqual "solution 1" 2572),
      TestCase (solution2 >>= assertEqual "solution 2" 2631)
    ]
