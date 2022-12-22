{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC20 where

import AOC (Solution (PureSolution))
import Data.List (delete, elemIndex, foldl')
import Text.Parsec (many, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

inputParser :: Parser [Int]
inputParser = many (negativeNumber <* newline)

mix :: [Int] -> [Int]
mix xsOriginal = foldl' mixit xsOriginal xsOriginal

mixit :: [Int] -> Int -> [Int]
mixit xs x =
  case elemIndex x xs of
    Nothing -> error "elem not found"
    Just idx ->
      let idx' = mod (idx + x) (pred $ length xs)
       in ( if idx == idx'
              then xs
              else take idx' (delete x xs) <> [x] <> drop idx' (delete x xs)
          )
            |> cycle
            |> drop idx
            |> take (length xs)

solve1 :: [Int] -> Int
solve1 xs =
  (mixed !! ((zeroIdx + 1000) `mod` length mixed))
    + (mixed !! ((zeroIdx + 2000) `mod` length mixed))
    + (mixed !! ((zeroIdx + 3000) `mod` length mixed))
  where
    mixed = mix xs
    zeroIdx = case elemIndex 0 mixed of
      Just idx -> idx
      Nothing -> error "no 0 found"


-- 1591
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve1

key :: Int
key = 811589153

solve2 :: [Int] -> Int
solve2 xs =
  (mixed !! ((zeroIdx + 1000) `mod` length mixed))
    + (mixed !! ((zeroIdx + 2000) `mod` length mixed))
    + (mixed !! ((zeroIdx + 3000) `mod` length mixed))
  where
    zeroIdx = case elemIndex 0 mixed of
      Just idx -> idx
      Nothing -> error "no 0 found"
    keyed = (* key) <$> xs
    mixed = foldl' mixit keyed keyed
          |> flip (foldl' mixit) keyed
          |> flip (foldl' mixit) keyed
          |> flip (foldl' mixit) keyed
          |> flip (foldl' mixit) keyed
          |> flip (foldl' mixit) keyed
          |> flip (foldl' mixit) keyed
          |> flip (foldl' mixit) keyed
          |> flip (foldl' mixit) keyed
          |> flip (foldl' mixit) keyed


-- too low 9843764836737
-- too low 8416991105763
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 1591 solution2 undefined

testData :: Input
testData =
  "1\n\
  \2\n\
  \-3\n\
  \3\n\
  \-2\n\
  \0\n\
  \4\n"
