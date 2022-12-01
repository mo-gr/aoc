{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC1 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), number)
import Text.Parsec (many1, newline)
import Data.List (sort)

inputParser :: Parser [[Int]]
inputParser = many1 (elfP <* newline)
  where elfP :: Parser [Int]
        elfP = many1 (number <* newline)

-- 69281
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> fmap sum
    |> maximum

-- 201524
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> fmap sum
    |> sort
    |> reverse
    |> take 3
    |> sum

solution :: Solution
solution = PureSolution solution1 69281 solution2 201524
