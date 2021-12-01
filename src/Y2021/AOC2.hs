module Y2021.AOC2 where

import Data.Either (fromRight)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (runP)
import Text.Parsec.ByteString (Parser)
import Util (Input, (|>))

inputParser :: Parser String
inputParser = undefined

solution1 :: Input -> Int
solution1 input =
  fromRight [] (runP inputParser () "" input)
    |> error "not yet"

solution2 :: Input -> Int
solution2 input =
  fromRight [] (runP inputParser () "" input)
    |> error "not yet"

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" undefined . solution1 =<< input,
      TestCase $ assertEqual "solution 2" undefined . solution2 =<< input
    ]
