{-# LANGUAGE OverloadedStrings #-}

module Y2015.AOC5 where

import Control.Monad (guard)
import Data.List (group, intersect, isInfixOf)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (letter, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

inputParser :: Parser [String]
inputParser = many1 (many1 letter <* newline)

filterNice :: [String] -> [String]
filterNice ws = do
  w <- ws
  guard $ intersect w "aeiou" |> length |> (>= 3)
  guard $ group w |> fmap length |> any (>= 2)
  guard $ not $ "ab" `isInfixOf` w
  guard $ not $ "cd" `isInfixOf` w
  guard $ not $ "pq" `isInfixOf` w
  guard $ not $ "xy" `isInfixOf` w
  pure w

hasRepeatingPairs, repeatusInterruptus :: String -> Bool
hasRepeatingPairs [] = False
hasRepeatingPairs (a : a' : rst) | [a, a'] `isInfixOf` rst = True
hasRepeatingPairs (_ : rst) = hasRepeatingPairs rst
repeatusInterruptus [] = False
repeatusInterruptus (a : _i : a' : _) | a == a' = True
repeatusInterruptus (_ : rst) = repeatusInterruptus rst

filterNicer :: [String] -> [String]
filterNicer ws = do
  w <- ws
  guard $ hasRepeatingPairs w
  guard $ repeatusInterruptus w
  pure w

-- 236
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filterNice
    |> length

-- 51
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> filterNicer
    |> length

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 236 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 51 . solution2 =<< input
    ]
