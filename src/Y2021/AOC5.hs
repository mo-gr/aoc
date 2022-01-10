{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC5 where

import AOC (Solution (PureSolution))
import Data.List (group, sort)
import Text.Parsec (char, many1, newline, sepBy, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Point = (Int, Int)

type Line = (Point, Point)

lineParser :: Parser Line
lineParser = do
  [x, y] <- sepBy number $ char ','
  _ <- string " -> "
  [x', y'] <- sepBy number $ char ','
  _ <- newline
  pure ((x, y), (x', y'))

inputParser :: Parser [Line]
inputParser = many1 lineParser

isHorizontalOrVertical :: Line -> Bool
isHorizontalOrVertical ((_, y), (_, y')) | y == y' = True
isHorizontalOrVertical ((x, _), (x', _)) | x == x' = True
isHorizontalOrVertical _ = False

nextClosest :: Int -> Int -> Int
nextClosest x y = case compare x y of
  LT -> x + 1
  EQ -> x
  GT -> x -1

expand :: Line -> [Point]
expand (p, p') | p == p' = [p]
expand ((x, y), (x', y')) =
  let px = nextClosest x x'
      py = nextClosest y y'
   in (x, y) : expand ((px, py), (x', y'))

countDuplicates :: [Line] -> Int
countDuplicates ls =
  fmap expand ls
    |> mconcat
    |> sort
    |> group
    |> filter ((> 1) . length)
    |> length

-- 7318
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filter isHorizontalOrVertical
    |> countDuplicates

-- 19939
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> countDuplicates

solution :: Solution
solution = PureSolution solution1 7318 solution2 19939
