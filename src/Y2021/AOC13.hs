{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC13 where

import AOC (Solution (PureStringSolution))
import Data.List (nub)
import Text.Parsec (char, many1, newline, string, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Point = (Int, Int)

data Fold = X Int | Y Int deriving (Show, Eq)

pointParser :: Parser Point
pointParser = do
  x <- number
  _ <- char ','
  y <- number
  _ <- newline
  pure (x, y)

foldParser :: Parser Fold
foldParser = do
  _ <- string "fold along "
  f <- (string "x=" *> (X <$> number)) <|> (string "y=" *> (Y <$> number))
  _ <- newline
  pure f

inputParser :: Parser ([Point], [Fold])
inputParser = do
  ps <- many1 pointParser
  _ <- newline
  fs <- many1 foldParser
  pure (ps, fs)

paperFold :: [Point] -> Fold -> [Point]
paperFold ps (Y y) = nub $ foldUp y <$> ps
paperFold ps (X x) = nub $ foldLeft x <$> ps

foldUp :: Int -> Point -> Point
foldUp foldLine (x, y) | y < foldLine = (x, y)
foldUp foldLine (_, y) | y == foldLine = error "something went wrong"
foldUp foldLine (x, y) = (x, foldLine - abs (foldLine - y))

foldLeft :: Int -> Point -> Point
foldLeft foldLine (x, y) | x < foldLine = (x, y)
foldLeft foldLine (x, _) | x == foldLine = error "something went wrong"
foldLeft foldLine (x, y) = (foldLine - abs (foldLine - x), y)

applyFirstFold :: ([Point], [Fold]) -> [Point]
applyFirstFold (ps, f : _) = paperFold ps f
applyFirstFold _ = error "something went wrong"

applyAllFolds :: ([Point], [Fold]) -> [Point]
applyAllFolds (ps, fs) = foldl paperFold ps fs

findMax :: Point -> [Point] -> Point
findMax p [] = p
findMax (x, y) ((x', y') : ps) = findMax (max x x', max y y') ps

pretty :: [Point] -> [String]
pretty ps =
  let (mx, my) = findMax (0, 0) ps
   in lines $ do
        y <- [0 .. my]
        x <- [0 .. mx]
        c <- if (x, y) `elem` ps then ['#'] else ['.']
        if x == mx then c : ['\n'] else [c]

solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> applyFirstFold
    |> length
    |> show

-- PZEHRAER
result :: [String]
result =
  [ "###..####.####.#..#.###...##..####.###.",
    "#..#....#.#....#..#.#..#.#..#.#....#..#",
    "#..#...#..###..####.#..#.#..#.###..#..#",
    "###...#...#....#..#.###..####.#....###.",
    "#....#....#....#..#.#.#..#..#.#....#.#.",
    "#....####.####.#..#.#..#.#..#.####.#..#"
  ]

solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> applyAllFolds
    |> pretty
    |> mconcat

solution :: Solution
solution = PureStringSolution solution1 "814" solution2 (mconcat result)
