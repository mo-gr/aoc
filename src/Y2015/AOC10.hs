module Y2015.AOC10 where

import AOC (Solution (PureSolution))
import Data.Functor ((<&>))
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (digit, many1)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, times, (|>))

inputParser :: Parser [Int]
inputParser = many1 (digit <&> (read . pure))

lookAndSay :: [Int] -> [Int]
lookAndSay [] = []
lookAndSay (s : ss) =
  let l = takeWhile (== s) ss |> length
   in (1 + l) : s : lookAndSay (drop l ss)

-- 360154
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> times 40 lookAndSay
    |> length

-- 5103798
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> times 50 lookAndSay
    |> length

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 360154 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 5103798 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 360154 solution2 5103798
