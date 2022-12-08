{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC8 (solution) where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import Data.List (transpose)
import Text.Parsec (digit, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

type Height = Int

inputParser :: Parser [[Height]]
inputParser = many1 (many1 (toInt <$> digit) <* newline)
  where
    toInt :: Char -> Int
    toInt = read . pure

canSeeEdge :: [Height] -> Bool
canSeeEdge [] = True
canSeeEdge (it : ts) = go it ts
  where
    go _ [] = True
    go t (t' : rest)
      | t <= t' = False
      | otherwise = go t rest

isVisible :: [[Height]] -> (Int, Int) -> Bool
isVisible trees (x, y) =
  let right = trees !! y |> drop x |> canSeeEdge
      left = trees !! y |> take (succ x) |> reverse |> canSeeEdge
      down = transpose trees !! x |> drop y |> canSeeEdge
      up = transpose trees !! x |> take (succ y) |> reverse |> canSeeEdge
   in right || left || down || up

countVisible :: [[Height]] -> Int
countVisible trees = length $ do
  x <- [0 .. (pred . length) trees]
  y <- [0 .. (pred . length) trees]
  guard $ isVisible trees (x, y)
  pure (x, y)

-- 1851
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> countVisible

viewingDist :: [Height] -> Int
viewingDist [] = 0
viewingDist (it : ts) = go it ts
  where
    go _ [] = 0
    go t (t' : rest)
      | t > t' = 1 + go t rest
      | otherwise = 1

scenicScore :: [[Height]] -> (Int, Int) -> Int
scenicScore trees (x, y) =
  let right = trees !! y |> drop x |> viewingDist
      left = trees !! y |> take (succ x) |> reverse |> viewingDist
      down = transpose trees !! x |> drop y |> viewingDist
      up = transpose trees !! x |> take (succ y) |> reverse |> viewingDist
   in right * left * down * up

maxScenicScore :: [[Height]] -> Int
maxScenicScore trees = maximum $ do
  x <- [0 .. (pred . length) trees]
  y <- [0 .. (pred . length) trees]
  pure $ scenicScore trees (x, y)

-- 574080
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> maxScenicScore

solution :: Solution
solution = PureSolution solution1 1851 solution2 574080
