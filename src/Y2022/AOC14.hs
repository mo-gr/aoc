{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC14 where

import AOC (Solution (PureSolution))
import qualified Data.Set as S
import Text.Parsec (char, many, newline, sepBy, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Point = (Int, Int)

type World = S.Set Point

inputParser :: Parser [[Point]]
inputParser = many $ do
  ps <- pointP `sepBy` string " -> "
  _ <- newline
  pure ps
  where
    pointP = do
      a <- number
      b <- char ',' *> number
      pure (a, b)

sandSource :: Point
sandSource = (500, 0)

cutOff :: Int
cutOff = 200

mkWorld :: [[Point]] -> World
mkWorld pps = S.fromList $ do
  ps <- pps
  ((x, y), (x', y')) <- zip ps (tail ps)
  px <- [(min x x') .. (max x x')]
  py <- [(min y y') .. (max y y')]
  pure (px, py)

dropSand :: World -> Point -> Maybe (Point, World)
dropSand w s@(x, y)
  | y > cutOff = Nothing
  | S.notMember (x, succ y) w = dropSand w (x, succ y)
  | S.notMember (pred x, succ y) w = dropSand w (pred x, succ y)
  | S.notMember (succ x, succ y) w = dropSand w (succ x, succ y)
  | s == sandSource = Nothing
  | otherwise = Just (s, S.insert s w)

flow :: World -> [Point]
flow w = case dropSand w sandSource of
  Just (p, w') -> p : flow w'
  Nothing -> []

-- 644
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> mkWorld
    |> flow
    |> length

addFloor :: World -> World
addFloor w = S.union w floorLine
  where
    yMax = S.map snd w |> S.findMax |> (+ 2)
    floorLine = S.fromList $ do
      x <- [(yMax * (-100)) .. yMax * 100]
      pure (x, yMax)

-- 27324
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> mkWorld
    |> addFloor
    |> flow
    |> length
    |> (+ 1) -- for the last sand grain

solution :: Solution
solution = PureSolution solution1 644 solution2 27324
