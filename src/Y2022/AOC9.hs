{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC9 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), number)
import Text.Parsec (many1, char, newline)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Control.Lens (over, _1, _2, view, Lens', set)
import Data.List (sort, nub)

data Direction = U | D | L | R
  deriving (Eq, Show)

type Point = (Int, Int)

type Rope = [Point]

inputParser :: Parser [Direction]
inputParser = fmap unwrapSteps $  many1 $ do
  d <- char 'R' $> R <|> char 'U' $> U <|> char 'L' $> L <|> char 'D' $> D
  s <- char ' ' *> number <* newline
  pure (d,s)

unwrapSteps :: [(Direction, Int)] -> [Direction]
unwrapSteps [] = []
unwrapSteps ((d,s):rest) = replicate s d <> unwrapSteps rest

x, y :: Lens' Point Int
x = _1
y = _2

move :: Rope -> Direction -> Rope
move [] _ = []
move (h:rope) d = scanl drag h' rope
  where h' = case d of
              U -> over y succ h
              D -> over y pred h
              R -> over x succ h
              L -> over x pred h

-- so ugly :(
drag :: Point -> Point -> Point
drag h t | view x h == view x t = case compare (view y h) (view y t) of
  EQ -> t
  GT -> set y (pred $ view y h) t
  LT -> set y (succ $ view y h) t
drag h t | view y h == view y t = case compare (view x h) (view x t) of
  EQ -> t
  GT -> set x (pred $ view x h) t
  LT -> set x (succ $ view x h) t
drag h t | view x h - view x t >= 2 =
  if view y h > view y t then (pred $ view x h, succ $ view y t) else (pred $ view x h, pred $ view y t)
drag h t | view x h - view x t <= -2 =
  if view y h > view y t then (succ $ view x h, succ $ view y t) else (succ $ view x h, pred $ view y t)
drag h t | view y h - view y t >= 2 =
  if view x h > view x t then (succ $ view x t, pred $ view y h) else (pred $ view x t, pred $ view y h)
drag h t | view y h - view y t <= -2 =
  if view x h > view x t then (succ $ view x t, succ $ view y h) else (pred $ view x t, succ $ view y h)
drag _ t = t

-- 6090
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> scanl move (replicate 2 (0,0))
    |> fmap last
    |> sort
    |> nub
    |> length

-- 2566
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
      |> scanl move (replicate 10 (0,0))
      |> fmap last
      |> sort
      |> nub
      |> length

solution :: Solution
solution = PureSolution solution1 6090 solution2 2566