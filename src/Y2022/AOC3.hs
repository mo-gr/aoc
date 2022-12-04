{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC3 where

import AOC (Solution (PureSolution))
import Data.Char (isUpper)
import qualified Data.Set as S
import Text.Parsec (letter, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

inputParser :: Parser [String]
inputParser = many1 (many1 letter <* newline)

splitSet :: String -> (S.Set Char, S.Set Char)
splitSet s = (S.fromList (take half s), S.fromList (drop half s))
  where
    half = length s `div` 2

common :: (S.Set Char, S.Set Char) -> S.Set Char
common (s, s') = S.intersection s s'

points :: Char -> Int
points c | isUpper c = 27 + fromEnum c - fromEnum 'A'
points c = 1 + fromEnum c - fromEnum 'a'

-- 7850
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> fmap splitSet
    |> fmap common
    |> fmap (head . S.toList)
    |> fmap points
    |> sum

threes :: [a] -> [(a, a, a)]
threes [] = []
threes (a : b : c : rest) = (a, b, c) : threes rest
threes _ = error "no threes grouping"

badge :: (String, String, String) -> Char
badge (a, b, c) =
  S.intersection (S.intersection (S.fromList a) (S.fromList b)) (S.fromList c)
    |> S.toList
    |> head

-- 2581
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> threes
    |> fmap badge
    |> fmap points
    |> sum

solution :: Solution
solution = PureSolution solution1 7850 solution2 2581
