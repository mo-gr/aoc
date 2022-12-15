{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y2022.AOC15 where

import AOC (Solution (PureSolution))
import Control.Lens (Lens', folded, makeLenses, toListOf, view, _1, _2)
import Control.Monad (guard)
import Data.List (sortOn)
import qualified Data.Set as S
import Text.Parsec (many, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

type Point = (Int, Int)

data Sensor = Sensor
  { _location :: Point,
    _beacon :: Point
  }

makeLenses ''Sensor

px, py :: Lens' Point Int
px = _1
py = _2

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

clearInLine :: Int -> Sensor -> [(Int, Int)]
clearInLine line s = do
  let dist = manhattanDistance (view location s) (view beacon s)
  let dy = abs (line - view (location . py) s)
  guard $ dy <= dist
  pure (view (location . px) s - dist + dy, view (location . px) s + dist - dy)

inputParser :: Parser [Sensor]
inputParser = many $ do
  sx <- string "Sensor at x=" *> negativeNumber
  sy <- string ", y=" *> negativeNumber
  bx <- string ": closest beacon is at x=" *> negativeNumber
  by <- string ", y=" *> negativeNumber <* newline
  pure $ Sensor (sx, sy) (bx, by)

countFieldsInRanges :: [(Int, Int)] -> Int
countFieldsInRanges rs = go (-9999999999999) $ sortOn fst rs
  where
    go _ [] = 0
    go current ((minR, maxR) : rest) = succ (maxR - max current minR) + go (succ maxR) (dropWhile (\(_, m) -> m < maxR) rest)

freeFieldsInRanges :: Int -> [(Int, Int)] -> [Int]
freeFieldsInRanges lim rs = go 0 $ sortOn fst rs
  where
    go current [] = [current .. lim]
    go current ((minR, maxR) : rest) =
      [current .. (pred minR)] <> go (succ maxR) (dropWhile (\(_, m) -> m < maxR) rest)

possibleInLine :: Int -> [Sensor] -> [Int]
possibleInLine lineNo ss = concatMap (clearInLine lineNo) ss |> freeFieldsInRanges 4000000

tuningFrequency :: [Point] -> Int
tuningFrequency [(x, y)] = x * 4000000 + y
tuningFrequency ps = error $ "not expecting " <> show ps

solve1, solve2 :: [Sensor] -> Int
solve1 ss =
  concatMap (clearInLine lineNo) ss
    |> countFieldsInRanges
    |> (\c -> c - beaconsInLine)
  where
    beaconsInLine = toListOf (folded . beacon) ss |> S.fromList |> S.toList |> filter ((== lineNo) . snd) |> length
    lineNo = 2000000
solve2 ss = tuningFrequency $ do
  y <- [0 .. 4000000]
  x <- possibleInLine y ss
  pure (x, y)

-- 5335787
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve1

-- 13673971349056
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 5335787 solution2 13673971349056