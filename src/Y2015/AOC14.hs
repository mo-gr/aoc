{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Y2015.AOC14 where

import AOC (Solution (IOSolution))
import Apecs
import Apecs.System (cmapIf)
import Control.Monad (forM_)
import Text.Parsec (letter, many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie)

newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity (Int, Int, Int) deriving (Show)

newtype Stamina = Stamina Int deriving (Show)

newtype Resting = Resting Int deriving (Show)

newtype Score = Score Int deriving (Show)

makeWorldAndComponents "World" [''Position, ''Velocity, ''Stamina, ''Resting, ''Score]

inputParser :: Parser [(Int, Int, Int)]
inputParser = many1 $ do
  _name <- many1 letter
  _ <- string " can fly "
  vMax <- number
  _ <- string " km/s for "
  stami <- number
  _ <- string " seconds, but then must rest for "
  resting <- number
  _ <- string " seconds." <* newline
  pure (vMax, stami, resting)

createWorld :: [(Int, Int, Int)] -> System World ()
createWorld rs = do
  forM_ rs $ \r@(_, stamina, _) -> newEntity (Position 0, Velocity r, Stamina stamina, Score 0)

tick :: System World ()
tick = do
  cmap (\(Position p, Velocity (v, _, _), Not :: Not Resting) -> Position $ p + v)
  cmap (\(Resting r) -> Resting (pred r))
  cmap (\(Stamina s) -> Stamina (pred s))
  cmapIf (\(Stamina s) -> s == 0) (\(Velocity (_, _, rest)) -> (Resting rest, Not :: Not Stamina))
  cmapIf (\(Resting r) -> r == 0) (\(Velocity (_, stam, _)) -> (Stamina stam, Not :: Not Resting))
  maxDist' <- maxDist
  cmapIf (\(Position p) -> p == maxDist') (\(Score s) -> Score $ s + 1)

simulate :: Int -> [(Int, Int, Int)] -> System World Int -> IO Int
simulate ticks rs extract =
  initWorld
    >>= runSystem
      ( do
          createWorld rs
          forM_ [1 .. ticks] $ const tick
          extract
      )

maxScore, maxDist :: System World Int
maxScore = cfold (\mScore (Score s) -> max mScore s) 0
maxDist = cfold (\mDist (Position p) -> max mDist p) 0

-- 2640
solution1 :: Input -> IO Int
solution1 input = do
  let rs = parseOrDie inputParser input
  simulate 2503 rs maxDist

-- 1102
solution2 :: Input -> IO Int
solution2 input = do
  let rs = parseOrDie inputParser input
  simulate 2503 rs maxScore

solution :: Solution
solution = IOSolution solution1 2640 solution2 1102
