{-# LANGUAGE OverloadedStrings #-}

module Y2015.AOC25 where

import AOC (Solution (PureSolution))
import Util (Input)

type Coord = (Int, Int)

flatten :: Coord -> Int
flatten (1, 1) = 1
flatten (n, 1) = 1 + sum [1 .. (n - 1)]
flatten (n, m) = 1 + flatten (n + 1, m - 1)

codes :: [Int]
codes = iterate f 20151125
  where
    f n = (n * 252533) `mod` 33554393

-- 9132360
solution1 :: Input -> Int
solution1 _input = codes !! pred (flatten input) -- pred to go from 1 based to 0 based indexing

solution2 :: Input -> Int
solution2 _input = 0

input :: Coord
input = (2981, 3075)

solution :: Solution
solution = PureSolution solution1 9132360 solution2 0
