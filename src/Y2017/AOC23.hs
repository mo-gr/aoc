{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC23 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

inputParser :: Parser Int
inputParser = undefined

solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> undefined

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> undefined

solution :: Solution
solution = PureSolution solution1 undefined solution2 undefined
