{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC18 where

import AOC (Solution (PureSolution))
import Control.Lens (Lens', view, _1, _2, _3)
import Control.Monad (guard)
import qualified Data.Set as S
import Text.Parsec (char, many, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Cube = (Int, Int, Int)

lx, ly, lz :: Lens' Cube Int
lx = _1
ly = _2
lz = _3

inputParser :: Parser [Cube]
inputParser = many $ do
  cx <- number <* char ','
  cy <- number <* char ','
  cz <- number <* newline
  pure (cx, cy, cz)

countSides :: S.Set Cube -> Cube -> Int
countSides cubes (x, y, z) = length $ do
  c' <- [(succ x, y, z), (x, succ y, z), (x, y, succ z), (pred x, y, z), (x, pred y, z), (x, y, pred z)]
  guard $ S.notMember c' cubes
  pure c'

countOutsides :: S.Set Cube -> S.Set Cube -> Cube -> Int
countOutsides cubes outsides (x, y, z) = length $ do
  c' <- [(succ x, y, z), (x, succ y, z), (x, y, succ z), (pred x, y, z), (x, pred y, z), (x, y, pred z)]
  guard $ S.notMember c' cubes
  guard $ S.member c' outsides
  pure c'

minMax :: [Int] -> (Int, Int)
minMax xs = (pred $ minimum xs, succ $ maximum xs)

findOutside :: [Cube] -> S.Set Cube
findOutside cs = go $ S.singleton (xmin, ymin, zmin)
  where
    (xmin, xmax) = minMax $ view lx <$> cs
    (ymin, ymax) = minMax $ view ly <$> cs
    (zmin, zmax) = minMax $ view lz <$> cs
    css = S.fromList cs
    go outside = case S.unions (outside : (neighbours <$> S.toList outside)) of
      outside' | outside' == outside -> outside
      outside' -> go outside'
    neighbours (x, y, z) =
      S.fromList $ do
        c'@(x', y', z') <- [(succ x, y, z), (x, succ y, z), (x, y, succ z), (pred x, y, z), (x, pred y, z), (x, y, pred z)]
        guard $ S.notMember c' css
        guard $ x' >= xmin && x' <= xmax
        guard $ y' >= ymin && y' <= ymax
        guard $ z' >= zmin && z' <= zmax
        pure c'

solve1 :: [Cube] -> Int
solve1 cs = sum $ fmap (countSides (S.fromList cs)) cs

-- 4636
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve1

solve2 :: [Cube] -> Int
solve2 cs = sum $ fmap (countOutsides (S.fromList cs) (findOutside cs)) cs

-- 2572
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 4636 solution2 2572
