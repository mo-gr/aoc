{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC22 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Bifunctor (first, second)
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Text.Parsec (char, getParserState, many, newline, sepBy, sourceColumn, sourceLine, statePos, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Point = (Int, Int)

data Move = Step | L | R

data Field = Wall | Open | None

data Facing = N | S | E | W

instance Show Move where
  show L = "L"
  show R = "R"
  show Step = "S"

instance Show Field where
  show Wall = "#"
  show Open = "."
  show None = " "

instance Show Facing where
  show N = "^"
  show S = "v"
  show E = ">"
  show W = "<"

fieldParser :: Parser [(Point, Field)]
fieldParser = fmap concat $
  flip sepBy newline $
    many $ do
      sp <- statePos <$> getParserState
      f <- (char ' ' $> None) <|> (char '.' $> Open) <|> (char '#' $> Wall)
      pure ((sourceColumn sp, sourceLine sp), f)

directionsParser :: Parser [Move]
directionsParser = do
  firstSteps <- number
  rest <- fmap concat $
    many $
      try $ do
        dir <- (char 'L' $> L) <|> (char 'R' $> R)
        cnt <- number
        pure $ [dir] <> replicate cnt Step
  pure $ replicate firstSteps Step <> rest

inputParser :: Parser (M.Map Point Field, [Move])
inputParser = do
  field <- fieldParser
  directions <- directionsParser
  pure (M.fromList field, directions)

findStart :: M.Map Point Field -> (Point, Facing)
findStart m = head $ do
  x <- [1 ..]
  let p = (x, 1)
  case M.lookup p m of
    Just Open -> [(p, E)]
    _ -> []

turn :: Facing -> Move -> Facing
turn f Step = f
turn E L = N
turn E R = S
turn N L = W
turn N R = E
turn W L = S
turn W R = N
turn S L = E
turn S R = W

next :: M.Map Point Field -> (Point, Facing) -> Point
next w (p, f) = case M.lookup p' w of
  Just Open -> p'
  Just Wall -> p
  _ -> wrap p p'
  where
    p' = case f of
      S -> second succ p
      N -> second pred p
      E -> first succ p
      W -> first pred p
    afterWrap :: [Point] -> Point
    afterWrap [] = error "no open"
    afterWrap (mo : prest) = case M.lookup mo w of
      Just Open -> mo
      Just Wall -> p
      _ -> afterWrap prest
    wrap (x, y) (x', y') | x == x' && y < y' = afterWrap [(x, yy) | yy <- [1 .. 250]]
    wrap (x, y) (x', y') | x == x' && y > y' = afterWrap [(x, yy) | yy <- [250, 249 .. 1]]
    wrap (x, y) (x', y') | y == y' && x < x' = afterWrap [(xx, y) | xx <- [1 .. 250]]
    wrap (x, y) (x', y') | y == y' && x > x' = afterWrap [(xx, y) | xx <- [250, 249 .. 1]]
    wrap _ _ = error "invalid wrap"

move :: M.Map Point Field -> (Point, Facing) -> Move -> (Point, Facing)
move _w (p, f) L = (p, turn f L)
move _w (p, f) R = (p, turn f R)
move w (p, f) Step = (next w (p, f), f)

move3d :: M.Map Point Field -> (Point, Facing) -> Move -> (Point, Facing)
move3d _w (p, f) L = (p, turn f L)
move3d _w (p, f) R = (p, turn f R)
move3d w (p, f) Step = next3d w p f

next3d :: M.Map Point Field -> Point -> Facing -> (Point, Facing)
next3d w p f = case M.lookup p3d w of
  Just Wall -> (p, f)
  Just Open -> (p3d, f3d)
  _ -> error $ "invalid 3d movement from " <> show p <> " to " <> show p3d <> " (via " <> show p' <> ")"
  where
    p' = case f of
      S -> second succ p
      N -> second pred p
      E -> first succ p
      W -> first pred p
    (p3d, f3d) = step3d p p' f

{-
My cube:
    1   51  101 151
  1     111 222
        111 222
        111 222
 51     333
        333
        333
101 555 444
    555 444
    555 444
151 666
    666
    666
-}

step3d :: Point -> Point -> Facing -> (Point, Facing)
step3d (x, y) (x', y') f
  | x' < 101 && y' == 0 = ((1, 100 + x), E) -- 1 > 6
  | x' == 0 && y' > 150 = ((y - 100, 1), S) -- 6 > 1
  | x' == 50 && y' < 51 = ((1, 151 - y), E) -- 1 > 5
  | x' > 100 && y' == 51 = ((100, x - 50), W) -- 2 > 3
  | x' == 101 && y' > 50 && y' <= 100 = ((y + 50, 50), N) -- 3 > 2
  | x' > 150 = ((100, 151 - y), W) -- 2 > 4
  | x' == 101 && y' > 100 = ((150, 151 - y), W) -- 4 > 2
  | x' <= 50 && y' == 100 = ((51, 50 + x), E) -- 5 > 3
  | x' == 50 && y' > 50 && y' <= 100 = (((-50) + y, 101), S) -- 3 > 5
  | x' > 100 && y' == 0 = ((x - 100, 200), N) -- 2 > 6
  | x' == 51 && y' > 150 = ((y - 100, 150), N) -- 6 > 4
  | x' > 50 && y' == 151 = ((50, x + 100), W) -- 4 > 6
  | x' == 0 && y' < 151 = ((51, 151 - y), E) -- 5 > 1
  | x' < 51 && y' == 201 = ((x + 100, 1), S) -- 6 > 2
  | otherwise = ((x', y'), f)

score :: (Point, Facing) -> Int
score ((col, row), face) = 1000 * row + 4 * col + faceValue
  where
    faceValue = case face of
      N -> 3
      S -> 1
      E -> 0
      W -> 2

solve1, solve2 :: (M.Map Point Field, [Move]) -> Int
solve1 (world, directions) = score $ foldl (move world) (findStart world) directions
solve2 (world, directions) = score $ foldl (move3d world) (findStart world) directions

-- 73346
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve1

-- 106392
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve2

solution :: Solution
solution = PureSolution solution1 73346 solution2 106392
