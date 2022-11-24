{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC20 where

import AOC (Solution (PureSolution))
import Data.Function (on)
import Data.List (groupBy, minimumBy)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char, newline, string)
import Text.Parsec.Combinator (many1)
import Util (Input, negativeNumber, parseOrDie, times, (|>))

type Point = (Int, Int, Int)

data Particle = Particle
  { pos :: Point,
    vel :: Point,
    acc :: Point
  }
  deriving (Show, Eq)

inputParser :: Parser [Particle]
inputParser = many1 $ do
  px <- string "p=<" *> negativeNumber
  py <- char ',' *> negativeNumber
  pz <- char ',' *> negativeNumber
  vx <- string ">, v=<" *> negativeNumber
  vy <- char ',' *> negativeNumber
  vz <- char ',' *> negativeNumber
  ax <- string ">, a=<" *> negativeNumber
  ay <- char ',' *> negativeNumber
  az <- char ',' *> negativeNumber
  _ <- string ">" *> newline
  pure $ Particle (px, py, pz) (vx, vy, vz) (ax, ay, az)

tick :: Particle -> Particle
tick Particle {pos = (px, py, pz), vel = (vx, vy, vz), acc = (ax, ay, az)} =
  Particle (px + vx + ax, py + vy + ay, pz + vz + az) (vx + ax, vy + ay, vz + az) (ax, ay, az)

distanceZero :: Particle -> Int
distanceZero Particle {pos = (px, py, pz)} = abs px + abs py + abs pz

-- 308
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> times 400 (fmap tick) -- stable through experimentation
    |> zip [0 ..]
    |> minimumBy (compare `on` abs . distanceZero . snd)
    |> fst

removeCollisions :: [Particle] -> [Particle]
removeCollisions ps = groupBy ((==) `on` pos) ps |> filter ((== 1) . length) |> concat

-- 504
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> times 100 (removeCollisions . fmap tick)
    |> length

solution :: Solution
solution = PureSolution solution1 308 solution2 504
