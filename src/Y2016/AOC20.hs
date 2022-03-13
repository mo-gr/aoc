{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC20 (solution) where

import AOC (Solution (PureSolution))
import Data.List (sortOn)
import Text.Parsec (char, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Range = Range {lower :: Int, upper :: Int} deriving (Eq, Show)

inputParser :: Parser [Range]
inputParser = many1 $ do
  Range <$> number <*> (char '-' *> number <* newline)

sortRanges :: [Range] -> [Range]
sortRanges = sortOn lower

hasOverlap :: Range -> Range -> Bool
hasOverlap r1 r2
  | upper r1 >= lower r2 = True
  | succ (upper r1) == lower r2 = True -- no room between the two, thus they technically overlap
  | otherwise = False

compact :: [Range] -> [Range]
compact = pairwiseMerge . sortRanges
  where
    pairwiseMerge :: [Range] -> [Range]
    pairwiseMerge [] = []
    pairwiseMerge [r] = [r]
    pairwiseMerge (r1 : r2 : rst)
      | hasOverlap r1 r2 = pairwiseMerge (Range (min (lower r1) (lower r2)) (max (upper r1) (upper r2)) : rst)
      | otherwise = r1 : pairwiseMerge (r2 : rst)

blockCount :: Range -> Int
blockCount r = succ (upper r - lower r)

-- 4793564
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> compact
    |> succ . upper . head

-- 146
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> compact
    |> (succ 4294967295 -) . sum . fmap blockCount

solution :: Solution
solution = PureSolution solution1 4793564 solution2 146
