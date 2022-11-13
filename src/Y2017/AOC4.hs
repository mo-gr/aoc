{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC4 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import Text.Parsec (sepBy1, many1, letter, newline, char)
import qualified Data.Set as S
import Data.List (sort)

inputParser :: Parser [[String]]
inputParser = many1 ((many1 letter `sepBy1` char ' ') <* newline)

noDuplicates :: [String] -> Bool
noDuplicates ws = S.size (S.fromList ws) == length ws

noAnagram :: [String] -> Bool
noAnagram ws = fmap sort ws |> noDuplicates

-- 386
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filter noDuplicates
    |> length

-- 208
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> filter noDuplicates
    |> filter noAnagram
    |> length

solution :: Solution
solution = PureSolution solution1 386 solution2 208
