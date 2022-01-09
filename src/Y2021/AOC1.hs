module Y2021.AOC1 where

import AOC (Solution(PureSolution))
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (newline, sepBy1)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Measurement = Int

inputParser :: Parser [Measurement]
inputParser = sepBy1 number newline

countIncreases :: [Measurement] -> Int
countIncreases (m : m' : ms)
  | m < m' = 1 + countIncreases (m' : ms)
  | otherwise = countIncreases (m' : ms)
countIncreases _ = 0

tripletSum :: Num a => [a] -> [a]
tripletSum xs = sum3 <$> zip3 xs (tail xs) (tail $ tail xs)
  where
    sum3 (a, b, c) = a + b + c

-- 1162
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> countIncreases

-- 1190
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> tripletSum
    |> countIncreases

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 1162 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 1190 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 solution2 verify
