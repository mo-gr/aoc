{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC6 where

import AOC (Solution (PureSolution))
import qualified Data.Set as S
import Text.Parsec (letter, many1)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

inputParser :: Parser String
inputParser = many1 letter

areFirstNDistinct :: Ord a => Int -> [a] -> Bool
areFirstNDistinct n = (== n) . length . S.fromList . take n

findDistinctOffset :: Int -> Int -> String -> Int
findDistinctOffset prefixLength idx stream
  | areFirstNDistinct prefixLength stream = idx + prefixLength
  | otherwise = findDistinctOffset prefixLength (succ idx) (tail stream)

markerPos, messagePos :: String -> Int
markerPos = findDistinctOffset 4 0
messagePos = findDistinctOffset 14 0

-- 1343
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> markerPos

-- 2193
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> messagePos

solution :: Solution
solution = PureSolution solution1 1343 solution2 2193
