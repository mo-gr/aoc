{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC1 where

import AOC (Solution (PureSolution))
import Text.Parsec (digit, many1)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

solve :: [Int] -> Int
solve lst = go (take (succ (length lst)) $ cycle lst)
  where
    go [] = 0
    go (x : xx : rest) | x == xx = x + go (xx : rest)
    go (_ : rest) = go rest

solve' :: [Int] -> Int
solve' lst' = go (length lst' `div` 2) (cycle lst') lst'
  where
    go _ _ [] = 0
    go offset lst (x : rest) | x == lst !! offset = x + go offset (tail lst) rest
    go offset lst rest = go offset (tail lst) (tail rest)

inputParser :: Parser [Int]
inputParser = many1 $ read . (: []) <$> digit

-- 1029
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve

-- 1220
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve'

solution :: Solution
solution = PureSolution solution1 1029 solution2 1220
