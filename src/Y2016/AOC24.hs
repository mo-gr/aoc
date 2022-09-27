{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC24 where

import AOC (Solution (PureSolution))
import AStar (aStar)
import Control.Applicative ((<|>))
import Control.Monad (forM)
import Control.Monad.State (State, evalState, get, put)
import Data.List (permutations)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Parsec (char, digit, getParserState, many1, newline, sourceColumn, sourceLine, statePos)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

type Point = (Int, Int)

data World = World Point [Point] (S.Set Point) deriving (Show)

instance Semigroup World where
  (World p ps s) <> (World p' ps' s') = World (if p > p' then p else p') (ps <> ps') (S.union s s')

instance Monoid World where
  mempty = World (0, 0) [] S.empty

inputParser :: Parser World
inputParser =
  mconcat <$> do
    many1 (wallP <|> floorP <|> goalP <|> (newline *> mempty))
  where
    wallP = char '#' *> mempty
    floorP = do
      sp <- statePos <$> getParserState
      _ <- char '.'
      pure $ World (0, 0) [] (S.singleton (sourceColumn sp, sourceLine sp))
    goalP = do
      sp <- statePos <$> getParserState
      d <- digit
      pure $ case d of
        '0' -> World (sourceColumn sp, sourceLine sp) [] $ S.singleton (sourceColumn sp, sourceLine sp)
        _ -> World (0, 0) [(sourceColumn sp, sourceLine sp)] $ S.singleton (sourceColumn sp, sourceLine sp)

manhatten :: Point -> Point -> Int
manhatten (x, y) (x', y') = abs (x - x') + abs (y - y')

neighbours :: S.Set Point -> Point -> S.Set Point
neighbours world (x, y) = S.intersection world $ S.fromList [(pred x, y), (x, pred y), (succ x, y), (x, succ y)]

type Memo = M.Map (Point, Point) [Point]

type MemoM = State Memo

pathFromTo :: S.Set Point -> Point -> Point -> MemoM [Point]
pathFromTo worldMap from to = do
  memo <- get
  case M.lookup (from, to) memo of
    Just p -> pure p
    Nothing -> case aStar graph dist heur goal from of
      Just path -> do
        put (M.insert (to, from) (reverse path) $ M.insert (from, to) path memo)
        pure path
      Nothing -> error "no path"
  where
    graph n = neighbours worldMap n
    dist _ _ = 1
    heur n = manhatten n to
    goal n = n == to

totalPath :: S.Set Point -> [Point] -> MemoM [Point]
totalPath _ [] = pure []
totalPath _ [_] = pure [] -- only one point left, no where to go
totalPath worldMap (f : t : rest) = do
  path <- pathFromTo worldMap f t
  others <- totalPath worldMap (t : rest)
  pure (path <> others)

solve :: World -> MemoM Int
solve (World s goals worldMap) =
  fmap minimum $
    fmap length <$> do
      let goalPerms = permutations goals
      forM goalPerms (\goalPerm -> totalPath worldMap (s : goalPerm))

solveReturn :: World -> MemoM Int
solveReturn (World s goals worldMap) =
  fmap minimum $
    fmap length <$> do
      let goalPerms = permutations goals
      forM goalPerms (\goalPerm -> totalPath worldMap (s : goalPerm <> [s]))

-- 430
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> \w -> evalState (solve w) M.empty

-- 700
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> \w -> evalState (solveReturn w) M.empty

solution :: Solution
solution = PureSolution solution1 430 solution2 700
