{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC22 where

import AOC (Solution (PureSolution))
import Control.Applicative (liftA2, (<|>))
import Data.Bifunctor (bimap)
import qualified Data.Set as S
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, newline)
import Text.Parsec.Prim (try)
import Text.ParserCombinators.Parsec (getParserState, many, sourceColumn, sourceLine, statePos)
import Util (Input, parseOrDie, times, (|>))

type Point = (Int, Int)

inputParser :: Parser [Point]
inputParser = many $
  try $ do
    _ <- many (char '.' <|> newline)
    sp <- statePos <$> getParserState
    _ <- char '#'
    pure (sourceColumn sp, sourceLine sp)

adjustedInfected :: [Point] -> (S.Set Point, Point)
adjustedInfected ps = (S.map adjust infectedSet, adjust carrierPos)
  where
    maxCol = maximum $ fst <$> ps
    maxRow = maximum $ snd <$> ps
    carrierPos = (succ (maxCol `div` 2), succ (maxRow `div` 2))
    infectedSet = S.fromList ps
    adjust = bimap (subtract (fst carrierPos)) (subtract (snd carrierPos))

data Direction = North | South | West | East deriving (Eq)

data State = State
  { carrier :: Point,
    orientation :: Direction,
    infectionCount :: Int,
    infected :: S.Set Point,
    weakened :: S.Set Point,
    flagged :: S.Set Point
  }

mkState :: (S.Set Point, Point) -> State
mkState (infectedPos, c) = State c North 0 infectedPos S.empty S.empty

move :: State -> State
move st = st {carrier = carrier'}
  where
    (cx, cy) = carrier st
    carrier' = case orientation st of
      North -> (cx, pred cy)
      South -> (cx, succ cy)
      West -> (pred cx, cy)
      East -> (succ cx, cy)

burst :: State -> State
burst st =
  move $
    if currentInfected st
      then
        st
          { orientation = rotateR (orientation st),
            infected = S.delete (carrier st) (infected st)
          }
      else
        st
          { orientation = rotateL (orientation st),
            infected = S.insert (carrier st) (infected st),
            infectionCount = succ (infectionCount st)
          }

rotateL, rotateR :: Direction -> Direction
rotateL East = North
rotateL West = South
rotateL South = East
rotateL North = West
rotateR East = South
rotateR West = North
rotateR South = West
rotateR North = East

-- 5450
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> adjustedInfected
    |> mkState
    |> times 10000 burst
    |> infectionCount

currentInfected, currentFlagged, currentWeakened :: State -> Bool
currentInfected = liftA2 S.member carrier infected
currentFlagged = liftA2 S.member carrier flagged
currentWeakened = liftA2 S.member carrier weakened

burst' :: State -> State
burst' = move . infect . rotate
  where
    rotate st | currentWeakened st = st
    rotate st | currentInfected st = st {orientation = rotateR (orientation st)}
    rotate st | currentFlagged st = st {orientation = rotateR $ rotateR (orientation st)}
    rotate st = st {orientation = rotateL (orientation st)}
    infect st
      | currentWeakened st =
        st
          { infected = S.insert (carrier st) (infected st),
            weakened = S.delete (carrier st) (weakened st),
            infectionCount = succ (infectionCount st)
          }
    infect st | currentInfected st = st {flagged = S.insert (carrier st) (flagged st), infected = S.delete (carrier st) (infected st)}
    infect st | currentFlagged st = st {flagged = S.delete (carrier st) (flagged st)}
    infect st = st {weakened = S.insert (carrier st) (weakened st)}

-- 2511957
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> adjustedInfected
    |> mkState
    |> times 10000000 burst'
    |> infectionCount

solution :: Solution
solution = PureSolution solution1 5450 solution2 2511957
