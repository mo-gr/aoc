{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC13 where

import AOC (Solution (PureSolution))
import AStar (aStar)
import Control.Monad (guard)
import Data.Bits (popCount)
import qualified Data.Set as S
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

inputParser :: Parser Int
inputParser = number

type Point = (Int, Int)

isWall :: Int -> Point -> Bool
isWall magic (x, y) =
  (x * x + 3 * x + 2 * x * y + y + y * y)
    |> (+ magic)
    |> popCount
    |> odd

neighbours :: Int -> Point -> S.Set Point
neighbours magic (x, y) = S.fromList $ do
  y' <- [pred y, y, succ y]
  x' <- [pred x, x, succ x]
  guard $ y >= 0
  guard $ x >= 0
  guard $ manhattanDistance (x, y) (x', y') == 1
  guard $ not (isWall magic (x', y'))
  [(x', y')]

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

shortestPath :: Int -> Point -> Maybe [Point]
shortestPath magic goal =
  aStar
    (neighbours magic)
    (\_ _ -> 1)
    (manhattanDistance goal)
    (== goal)
    (1, 1)

locationsIn50 :: Int -> Int -> S.Set Point -> S.Set Point
locationsIn50 _magic 50 past = past
locationsIn50 magic n past =
  let ns = S.unions $ past : fmap (neighbours magic) (S.toList past)
   in locationsIn50 magic (succ n) ns

-- 82
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> \magic ->
      shortestPath magic (31, 39)
        |> \case
          Nothing -> error "no path"
          Just p -> length p

-- not 135 - too low
-- not 136 - too low
-- not 141 -- too high
-- not 137
-- 138
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> \magic ->
      locationsIn50 magic 1 (S.singleton (1, 1))
        |> length
        |> succ -- plus starting point

testData :: Input
testData = ""

solution :: Solution
solution = PureSolution solution1 82 solution2 138
