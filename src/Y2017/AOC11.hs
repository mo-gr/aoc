{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC11 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Monad.State (State, execState, modify)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import Text.Parsec (char, newline, sepBy1, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Direction
  = N
  | NE
  | SE
  | S
  | SW
  | NW
  deriving (Show, Eq)

type Point = (Int, Int)

inputParser :: Parser [Direction]
inputParser = (directionP `sepBy1` char ',') <* newline
  where
    directionP =
      try (string "ne" $> NE)
        <|> try (string "nw" $> NW)
        <|> (string "n" $> N)
        <|> try (string "se" $> SE)
        <|> try (string "sw" $> SW)
        <|> (string "s" $> S)

move :: Point -> Direction -> Point
move (x, y) N = (x, succ y)
move (x, y) S = (x, pred y)
move (x, y) NW = (pred x, succ y)
move (x, y) SE = (succ x, pred y)
move (x, y) NE = (succ x, y)
move (x, y) SW = (pred x, y)

hexManhatten :: Point -> Point -> Int
hexManhatten (x, y) (x', y') =
  let dx = x - x'
      dy = y - y'
   in if signum dx == signum dy
        then abs (dx + dy)
        else max (abs dx) (abs dy)

-- 877
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> foldl move (0, 0)
    |> hexManhatten (0, 0)

withMax :: Point -> Direction -> State Int Point
withMax pos dir = do
  let pos' = move pos dir
  modify (max (hexManhatten (0, 0) pos'))
  pure pos'

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> foldlM withMax (0, 0)
    |> flip execState 0

solution :: Solution
solution = PureSolution solution1 877 solution2 undefined
