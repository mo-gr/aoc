{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC12 where

import AOC (Solution (PureSolution))
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Parsec (getParserState, letter, many, sourceColumn, sourceLine, statePos, spaces)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import AStar (aStar)
import Data.Maybe (fromJust, catMaybes)
import Control.Monad (guard)

type Point = (Int, Int)

type Elevation = Char

type World = M.Map Point Elevation

inputParser :: Parser  (Point, Point, World)
inputParser = fmap extract $ many $ do
  sp <- statePos <$> getParserState
  e <- letter <* spaces
  pure ((sourceColumn sp, sourceLine sp), e)

extract :: [(Point, Elevation)] -> (Point, Point, World)
extract w = (start, end, M.fromList w')
  where
    w' = replace <$> w
    replace (p, 'S') = (p, 'a')
    replace (p, 'E') = (p, 'z')
    replace p = p
    start = case find ((== 'S') . snd) w of
      Just st -> fst st
      Nothing -> error "no start"
    end = case find ((== 'E') . snd) w of
      Just en -> fst en
      Nothing -> error "no end"

manhatten :: Point -> Point -> Int
manhatten (x, y) (x', y') = abs (x - x') + abs (y - y')

neighbour :: World -> Point -> S.Set Point
neighbour w (x,y) = S.fromList $ do
  p' <- [(succ x, y), (pred x, y), (x, succ y), (x, pred y)]
  guard $ M.member p' w
  let elP = fromEnum $ w M.! (x,y)
  let elP' = fromEnum $ w M.! p'
  guard $ elP' - elP <= 1
  pure p'

solve1 :: (Point, Point, World) -> Maybe Int
solve1 (start, end, world) = length <$> aStar
  (neighbour world)
  (\_ _ -> 1)
  (manhatten end)
  (== end)
  start

-- 339
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve1
    |> fromJust

findAs :: World -> [Point]
findAs = fmap fst . filter ((== 'a') . snd) . M.toList

-- 332
solve2 :: (Point, Point, World) -> Int
solve2 (_, end, world) = minimum . catMaybes $ (\s -> solve1 (s, end, world)) <$> findAs world

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 339 solution2 332
