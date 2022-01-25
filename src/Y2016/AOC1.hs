{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC1 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Text.Parsec (char, sepBy, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Step = L {dist :: Int} | R {dist :: Int}

data Face = N | E | S | W deriving (Enum, Show)

inputParser :: Parser [Step]
inputParser = do
  ((char 'R' *> number <&> R) <|> (char 'L' *> number <&> L)) `sepBy` string ", "

follow :: [Step] -> (Int, Int)
follow = snd . foldl f (N, (0, 0))
  where
    f (face, position) step = let face' = turn face step in (face', walk face' (dist step) position)

search :: Face -> [(Int, Int)] -> [Step] -> (Int, Int)
search _f loc [] = error $ "not found " <> show loc
search _f [] _ = error "not found"
search f (l : ll) (s : ss) =
  let face' = turn f s
      loc' = walk face' (dist s) l
      locs = reverse $ visited l loc'
   in case filter (`elem` ll) locs of
        [] -> search face' (locs <> (l : ll)) ss
        (t : _) -> t

visited :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
visited (x, y) (x', y') = do
  dy <- if y > y' then reverse [y' .. y] else [y .. y']
  dx <- if x > x' then reverse [x' .. x] else [x .. x']
  pure (dx, dy)

walk :: Face -> Int -> (Int, Int) -> (Int, Int)
walk N d (x, y) = (x, y + d)
walk S d (x, y) = (x, y - d)
walk E d (x, y) = (x + d, y)
walk W d (x, y) = (x - d, y)

turn :: Face -> Step -> Face
turn N (L _) = W
turn W (R _) = N
turn f (L _) = pred f
turn f (R _) = succ f

-- 288
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> follow
    |> \(x, y) -> abs x + abs y

-- 111
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> search N [(0, 0)]
    |> \(x, y) -> abs x + abs y

solution :: Solution
solution = PureSolution solution1 288 solution2 111
