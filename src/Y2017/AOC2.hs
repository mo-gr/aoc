{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC2 where

import AOC (Solution (PureSolution))
import Data.List (sort)
import Text.Parsec (char, many1, newline, sepBy1, tab, try, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

inputParser :: Parser [[Int]]
inputParser = many1 $
  try $ do
    xs <- number `sepBy1` (tab <|> char ' ')
    _ <- newline
    pure xs

checksum :: [Int] -> Int
checksum xs = maximum xs - minimum xs

evensum :: [Int] -> Int
evensum [] = error "no divisible numbers"
evensum (x : xs) = case filter (\xx -> x `rem` xx == 0) xs of
  (xx : _) -> x `div` xx
  _ -> evensum xs

-- 41919
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> fmap checksum
    |> sum

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> fmap (evensum . reverse . sort)
    |> sum

solution :: Solution
solution = PureSolution solution1 41919 solution2 undefined

testData :: Input
testData = "5 9 2 8\n9 4 7 3\n3 8 6 5\n"
