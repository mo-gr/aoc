module Y2015.AOC3 where

import AOC (Solution (PureSolution))
import Data.Functor (($>))
import Data.List (nub)
import Text.Parsec (char, many1, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Direction = North | South | East | West deriving (Show, Eq)

type Location = (Int, Int)

directionParser :: Parser Direction
directionParser = do
  char '>' $> East
    <|> char '<' $> West
    <|> char '^' $> North
    <|> char 'v' $> South

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
solution1 :: Input -> Int
solution1 input =
  do
    parseOrDie inputParser input
    |> flip run (0, 0)
    |> nub
    |> length

-- 2631
solution2 :: Input -> Int
solution2 input =
  do
    parseOrDie inputParser input
    |> dezip
    |> \(santa, robot) -> length . nub $ run santa (0, 0) ++ run robot (0, 0)

solution :: Solution
solution = PureSolution solution1 2572 solution2 2631
