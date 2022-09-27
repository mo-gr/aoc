{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC24 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import qualified Data.Set as S
import Text.Parsec (many1, char, newline, statePos, getParserState, sourceColumn, sourceLine, digit)
import Control.Applicative ((<|>))
import AStar (aStar)
import Data.List (permutations)

type Point = (Int, Int)

data World = World Point [Point] (S.Set Point) deriving (Show)

instance Semigroup World where
  (World p ps s) <> (World p' ps' s') = World (if p > p' then p else p') (ps <> ps') (S.union s s')
instance Monoid World where
  mempty = World (0,0) [] S.empty

inputParser :: Parser World
inputParser =  mconcat <$> do
  many1 (wallP <|> floorP <|> goalP <|> (newline *> mempty))
  where
    wallP = char '#' *> mempty
    floorP = do
      sp <- statePos <$> getParserState
      _ <- char '.'
      pure $ World (0,0) [] (S.singleton (sourceColumn sp, sourceLine sp))
    goalP = do
      sp <- statePos <$> getParserState
      d <- digit
      pure $ case d of
        '0' -> World (sourceColumn sp, sourceLine sp) [] $ S.singleton (sourceColumn sp, sourceLine sp)
        _ -> World (0,0) [(sourceColumn sp, sourceLine sp)] $ S.singleton (sourceColumn sp, sourceLine sp)

manhatten :: Point -> Point -> Int
manhatten (x, y) (x', y') = abs (x - x') + abs (y - y')

neighbours :: S.Set Point -> Point -> S.Set Point
neighbours world (x,y) = S.intersection world $ S.fromList [(pred x, y), (x, pred y), (succ x, y), (x, succ y)]

pathFromTo :: S.Set Point -> Point -> Point -> [Point]
pathFromTo worldMap from to = case aStar graph dist heur goal from of
    Just path -> path
    Nothing -> error "no path"
  where
    graph n = neighbours worldMap n
    dist _ _ = 1
    heur n = manhatten n to
    goal n = n == to

totalPath :: S.Set Point -> [Point] -> [Point]
totalPath _ [] = []
totalPath _ [_] = [] -- only one point left, no where to go
totalPath worldMap (f:t:rest) = pathFromTo worldMap f t <> totalPath worldMap (t:rest)

solve :: World -> Int
solve (World s goals worldMap) = minimum $ length <$> do
    goalPerm <- permutations goals
    [totalPath worldMap (s:goalPerm)]    

solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> undefined

solution :: Solution
solution = PureSolution solution1 undefined solution2 undefined

testData :: Input
testData = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########\n"