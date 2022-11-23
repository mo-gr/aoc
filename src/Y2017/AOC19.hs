{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC19 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Monad.State.Strict (State, execState, modify)
import Data.Bifunctor (second)
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Text.Parsec (char, getParserState, letter, many, sourceColumn, sourceLine, space, statePos, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Field = Path | Corner | Letter Char
  deriving (Show, Eq)

type Point = (Int, Int)

type World = M.Map Point Field

type LetterAndStepCollector a = State ([Char], Int) a

data Direction = North | South | West | East
  deriving (Show, Eq)

inputParser :: Parser [(Point, Field)]
inputParser = many $ try (many space *> (letterParser <|> cornerParser <|> pathParser))

letterParser, cornerParser, pathParser :: Parser (Point, Field)
letterParser = do
  sp <- statePos <$> getParserState
  p <- Letter <$> letter
  pure ((sourceColumn sp, sourceLine sp), p)
cornerParser = do
  sp <- statePos <$> getParserState
  p <- char '+' $> Corner
  pure ((sourceColumn sp, sourceLine sp), p)
pathParser = do
  sp <- statePos <$> getParserState
  p <- (char '|' <|> char '-') $> Path
  pure ((sourceColumn sp, sourceLine sp), p)

next :: Point -> Direction -> Point
next (x, y) North = (x, pred y)
next (x, y) South = (x, succ y)
next (x, y) East = (succ x, y)
next (x, y) West = (pred x, y)

turnTo :: World -> Point -> Direction -> Direction
turnTo w (x, y) South = fromJust $ (M.lookup (succ x, y) w >> pure East) <|> (M.lookup (pred x, y) w >> pure West)
turnTo w (x, y) North = fromJust $ (M.lookup (succ x, y) w >> pure East) <|> (M.lookup (pred x, y) w >> pure West)
turnTo w (x, y) West = fromJust $ (M.lookup (x, pred y) w >> pure North) <|> (M.lookup (x, succ y) w >> pure South)
turnTo w (x, y) East = fromJust $ (M.lookup (x, pred y) w >> pure North) <|> (M.lookup (x, succ y) w >> pure South)

walkTilCorner :: World -> Point -> Direction -> LetterAndStepCollector (Maybe (Point, Direction))
walkTilCorner w start dir = case M.lookup (next start dir) w of
  Nothing -> modify (second succ) >> pure Nothing
  Just Path -> modify (second succ) >> walkTilCorner w (next start dir) dir
  Just (Letter c) -> modify (\(l, s) -> (l <> [c], succ s)) >> walkTilCorner w (next start dir) dir
  Just Corner -> modify (second succ) >> pure (Just (next start dir, turnTo w (next start dir) dir))

findStart :: World -> Point
findStart w = firstJust $ (\x -> M.lookup (x, 1) w >> pure (x, 1)) <$> [1 ..]
  where
    firstJust :: [Maybe a] -> a
    firstJust ((Just x) : _) = x
    firstJust (Nothing : rest) = firstJust rest
    firstJust [] = error "no justice in this world"

solve :: World -> LetterAndStepCollector (Point, Direction)
solve w = walk w $ pure $ Just (findStart w, South)

walk :: World -> LetterAndStepCollector (Maybe (Point, Direction)) -> LetterAndStepCollector (Point, Direction)
walk w maybePrev = do
  prev <- maybePrev
  case prev of
    Just (start, dir) -> walk w (walkTilCorner w start dir)
    Nothing -> pure ((0, 0), North)

-- XYFDJNRCQA
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> M.fromList
    |> solve
    |> flip execState ([], 0)
    |> fst

-- 17450
solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> M.fromList
    |> solve
    |> flip execState ([], 0)
    |> snd
    |> show

solution :: Solution
solution = PureSolution solution1 "XYFDJNRCQA" solution2 "17450"
