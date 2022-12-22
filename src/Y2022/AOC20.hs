{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC20 where

import AOC (Solution (PureSolution))
import Data.List (delete, elemIndex, find, foldl')
import Text.Parsec (many, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

inputParser :: Parser [Int]
inputParser = many (negativeNumber <* newline)

mixit :: [(Int, Int)] -> Int -> [(Int, Int)]
mixit xs xx =
  let x = case snd <$> find ((== xx) . fst) xs of
        Just i -> i
        _ -> error "index not found"
   in case elemIndex (xx, x) xs of
        Nothing -> error "elem not found"
        Just idx ->
          let idx' = mod (idx + x) (pred $ length xs)
           in if idx == idx'
                then xs
                else take idx' (delete (xx, x) xs) <> [(xx, x)] <> drop idx' (delete (xx, x) xs)

solve1 :: [Int] -> Int
solve1 xs =
  (mixed !! ((zeroIdx + 1000) `mod` length mixed))
    + (mixed !! ((zeroIdx + 2000) `mod` length mixed))
    + (mixed !! ((zeroIdx + 3000) `mod` length mixed))
  where
    mixed = foldl' mixit (zip [0 ..] xs) [0 .. pred (length xs)] |> fmap snd
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
    idxes = [0 .. pred (length xs)]
    mixed =
      foldl' mixit (zip [0 ..] keyed) idxes
        |> flip (foldl' mixit) idxes
        |> flip (foldl' mixit) idxes
        |> flip (foldl' mixit) idxes
        |> flip (foldl' mixit) idxes
        |> flip (foldl' mixit) idxes
        |> flip (foldl' mixit) idxes
        |> flip (foldl' mixit) idxes
        |> flip (foldl' mixit) idxes
        |> flip (foldl' mixit) idxes
        |> fmap snd

-- 14579387544492
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 1591 solution2 14579387544492
