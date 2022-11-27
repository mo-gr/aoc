module Y2015.AOC17 where

import AOC (Solution (PureSolution))
import Control.Applicative
import Control.Monad (guard)
import Control.Monad.Logic
import Data.List (subsequences)
import Util (Input, (|>))

choose :: [a] -> Logic a
choose = foldr ((<|>) . pure) empty

waysToFill :: Int -> [Int] -> [[Int]]
waysToFill capacity containers = observeAll $ do
  selection <- choose $ subsequences containers
  guard $ sum selection == capacity
  pure selection

filterMinLength :: [[Int]] -> [[Int]]
filterMinLength xs = let minSize = minimum $ length <$> xs in filter ((== minSize) . length) xs

-- 1304
solution1 :: Input -> Int
solution1 _input =
  waysToFill 150 availableContainers
    |> length

-- 18
solution2 :: Input -> Int
solution2 _input =
  waysToFill 150 availableContainers
    |> filterMinLength
    |> length

testData, availableContainers :: [Int]
testData = [20, 15, 10, 5, 5]
availableContainers = [33, 14, 18, 20, 45, 35, 16, 35, 1, 13, 18, 13, 50, 44, 48, 6, 24, 41, 30, 42]

solution :: Solution
solution = PureSolution solution1 1304 solution2 18
