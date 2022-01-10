module Y2020.AOC1 where

import AOC (Solution (PureSolution))
import Data.List (find)
import Text.Parsec (many1, skipMany, space)
import Text.Parsec.ByteString (Parser)
import Util (number, Input, (|>), parseOrDie)
import Data.Maybe (fromJust)

parseExpenses :: Parser [Int]
parseExpenses = many1 (number <* skipMany space)

exampleInput :: [Int]
exampleInput =
  [ 978,
    979,
    366,
    1721,
    299,
    675,
    1456
  ]

mulExpenses :: Maybe (Int, Int) -> Maybe Int
mulExpenses (Just (x, y)) = Just (x * y)
mulExpenses _ = Nothing

mulExpenses3 :: Maybe (Int, Int, Int) -> Maybe Int
mulExpenses3 (Just (x, y, z)) = Just (x * y * z)
mulExpenses3 _ = Nothing

allPairs :: [a] -> [(a, a)]
allPairs ls@(_ : xs) = [(x, pair) | x <- ls, pair <- xs]
allPairs _ = []

allTriples :: [a] -> [(a, a, a)]
allTriples ls@(_ : xs@(_ : ys)) = [(x, pair, triple) | x <- ls, pair <- xs, triple <- ys]
allTriples _ = []

findSum2020 :: [Int] -> Maybe (Int, Int)
findSum2020 = find (\(x, y) -> x + y == 2020) . allPairs

findSum20203 :: [Int] -> Maybe (Int, Int, Int)
findSum20203 = find (\(x, y, z) -> x + y + z == 2020) . allTriples

-- 545379
solution1 :: Input -> Int
solution1 input = do
  parseOrDie parseExpenses input
    |> findSum2020
    |> mulExpenses
    |> fromJust

-- 257778836
solution2 :: Input -> Int
solution2 input = do
  parseOrDie parseExpenses input
      |> findSum20203
      |> mulExpenses3
      |> fromJust

solution :: Solution
solution = PureSolution solution1 545379 solution2 257778836
