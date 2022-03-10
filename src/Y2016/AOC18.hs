{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC18 (solution) where

import AOC (Solution (PureSolution))
import Data.Functor (($>))
import Text.Parsec (char, many1, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Tile = Safe | Trap deriving (Eq)

instance Show Tile where
  show Safe = "."
  show Trap = "^"

evolve :: Tile -> Tile -> Tile -> Tile
evolve Trap Trap Safe = Trap
evolve Safe Trap Trap = Trap
evolve Trap Safe Safe = Trap
evolve Safe Safe Trap = Trap
evolve _ _ _ = Safe

evolveLine :: [Tile] -> [Tile]
evolveLine ts = zipWith3 evolve (Safe : ts) ts (tail ts <> [Safe])

room :: [Tile] -> [[Tile]]
room t = t : room (evolveLine t)

inputParser :: Parser [Tile]
inputParser = many1 $ char '^' $> Trap <|> char '.' $> Safe

-- 1974
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> room
    |> take 40
    |> mconcat
    |> filter (== Safe)
    |> length

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> room
    |> take 400000
    |> mconcat
    |> filter (== Safe)
    |> length

solution :: Solution
solution = PureSolution solution1 1974 solution2 undefined
