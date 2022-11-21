{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC15 where

import AOC (Solution (PureSolution))
import Data.Word (Word16)
import Text.Parsec (newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

inputParser :: Parser (Int, Int)
inputParser = do
  ga <- string "Generator A starts with " *> number <* newline
  gb <- string "Generator B starts with " *> number <* newline
  pure (ga, gb)

factorA, factorB, divisor, moduloA, moduloB :: Int
factorA = 16807
factorB = 48271
divisor = 2147483647
moduloA = 4
moduloB = 8

generate :: Int -> Int -> Int
generate factor previous = previous * factor `rem` divisor

judge :: (Int, Int) -> Bool
judge (valA, valB) =
  let valA16, valB16 :: Word16
      valA16 = fromIntegral valA
      valB16 = fromIntegral valB
   in valA16 == valB16

toStream1, toStream2 :: (Int, Int) -> [(Int, Int)]
toStream1 (a, b) = zip (iterate (generate factorA) a) (iterate (generate factorB) b)
toStream2 (a, b) =
  zip
    (iterate (generate factorA) a |> filter ((0 ==) . flip mod moduloA))
    (iterate (generate factorB) b |> filter ((0 ==) . flip mod moduloB))

-- 638
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> toStream1
    |> take 40000000
    |> filter judge
    |> length

-- 343
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> toStream2
    |> take 5000000
    |> filter judge
    |> length

solution :: Solution
solution = PureSolution solution1 638 solution2 343
