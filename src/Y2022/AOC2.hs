{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC2 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Text.Parsec (char, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data ABC = A | B | C deriving (Eq, Show)

data XYZ = X | Y | Z deriving (Eq, Show)

data RPS = Rock | Paper | Scissors deriving (Eq, Show)

data Outcome = Lose | Draw | Win deriving (Eq, Show)

type Strategy = (ABC, XYZ)

inputParser :: Parser [Strategy]
inputParser = many1 $ do
  given <- char 'A' $> A <|> char 'B' $> B <|> char 'C' $> C
  _ <- char ' '
  response <- char 'X' $> X <|> char 'Y' $> Y <|> char 'Z' $> Z
  _ <- newline
  pure (given, response)

abcToRps :: ABC -> RPS
abcToRps A = Rock
abcToRps B = Paper
abcToRps C = Scissors

xyzToRps :: XYZ -> RPS
xyzToRps X = Rock
xyzToRps Y = Paper
xyzToRps Z = Scissors

xyzToOutcome :: XYZ -> Outcome
xyzToOutcome X = Lose
xyzToOutcome Y = Draw
xyzToOutcome Z = Win

valueRps :: RPS -> Int
valueRps Rock = 1
valueRps Paper = 2
valueRps Scissors = 3

matchPoints :: RPS -> RPS -> Int
matchPoints r r' | r == r' = 3
matchPoints Paper Rock = 6
matchPoints Rock Scissors = 6
matchPoints Scissors Paper = 6
matchPoints _ _ = 0

outcomePoints :: Outcome -> Int
outcomePoints Win = 6
outcomePoints Draw = 3
outcomePoints Lose = 0

outcomeToRps :: RPS -> Outcome -> RPS
outcomeToRps r Draw = r
outcomeToRps Rock Win = Paper
outcomeToRps Rock Lose = Scissors
outcomeToRps Paper Win = Scissors
outcomeToRps Paper Lose = Rock
outcomeToRps Scissors Win = Rock
outcomeToRps Scissors Lose = Paper

score :: (RPS, RPS) -> Int
score (r, r') = valueRps r + matchPoints r' r

-- 9651
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> fmap (bimap abcToRps xyzToRps)
    |> fmap score
    |> sum

-- 10560
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> fmap (bimap abcToRps xyzToOutcome)
    |> fmap (\(r, p) -> (p, outcomeToRps r p))
    |> fmap (bimap outcomePoints valueRps)
    |> fmap (uncurry (+))
    |> sum

solution :: Solution
solution = PureSolution solution1 9651 solution2 10560
