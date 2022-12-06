{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC6 where

import AOC (Solution (PureSolution))
import qualified Data.Set as S
import Text.Parsec (letter, many1)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

inputParser :: Parser String
inputParser = many1 letter

startDistinctOffset :: Int -> Int -> String -> Int
startDistinctOffset prefixLength idx stream
  | length (S.fromList (take prefixLength stream)) == prefixLength = idx + prefixLength
  | otherwise = startDistinctOffset prefixLength (succ idx) (tail stream)

markerEnd, messageStart :: String -> Int
markerEnd = startDistinctOffset 4 0
messageStart = startDistinctOffset 14 0

-- 1343
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> markerEnd

-- 2193
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> messageStart

solution :: Solution
solution = PureSolution solution1 1343 solution2 2193
