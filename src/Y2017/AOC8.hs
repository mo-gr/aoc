{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC8 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Monad.State.Strict (State, execState, modify)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Text.Parsec (letter, many1, newline, space, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

data Cond a
  = Eq a Int
  | Neq a Int
  | Lt a Int
  | Leq a Int
  | Gt a Int
  | Geq a Int

data Op a
  = Inc a Int (Cond a)
  | Dec a Int (Cond a)

type Register = String

type Memory = M.Map Register Int

opParser :: Parser (Op Register)
opParser = do
  reg <- many1 letter <* space
  op <- string "inc" $> Inc <|> string "dec" $> Dec
  val <- space *> negativeNumber <* space
  op reg val <$> conditionParser

conditionParser :: Parser (Cond Register)
conditionParser = do
  reg <- string "if " *> many1 letter <* space
  cond <-
    string "== " $> Eq
      <|> string "!= " $> Neq
      <|> try (string "< " $> Lt)
      <|> string "<= " $> Leq
      <|> try (string "> " $> Gt)
      <|> string ">= " $> Geq
  val <- negativeNumber <* newline
  pure $ cond reg val

inputParser :: Parser [Op Register]
inputParser = many1 opParser

eval :: Memory -> Op Register -> Memory
eval m (Inc addr n cond)
  | test m cond = M.insertWith (+) addr n m
  | otherwise = m
eval m (Dec addr n cond)
  | test m cond = M.insertWith (+) addr (negate n) m
  | otherwise = m

test :: Memory -> Cond Register -> Bool
test m (Eq addr val) = M.findWithDefault 0 addr m == val
test m (Neq addr val) = M.findWithDefault 0 addr m /= val
test m (Lt addr val) = M.findWithDefault 0 addr m < val
test m (Leq addr val) = M.findWithDefault 0 addr m <= val
test m (Gt addr val) = M.findWithDefault 0 addr m > val
test m (Geq addr val) = M.findWithDefault 0 addr m >= val

findMax :: Memory -> Int
findMax mem | M.null mem = 0
findMax mem = M.toList mem |> fmap snd |> maximum

-- 4902
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> foldl eval M.empty
    |> findMax

type WithMax m = State Int m

eval' :: Memory -> Op Register -> WithMax Memory
eval' mem m = do
  let mem' = eval mem m
  modify (max (findMax mem'))
  pure mem'

-- 7037
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> foldlM eval' M.empty
    |> flip execState 0

solution :: Solution
solution = PureSolution solution1 4902 solution2 7037

testData :: Input
testData = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n"
