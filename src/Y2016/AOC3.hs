{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC3 where

import AOC (Solution (PureSolution))
import Data.List (sort)
import Text.Parsec (many, many1, newline, space)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Tri = Tri Int Int Int

inputParser :: Parser [Tri]
inputParser = many1 $ do
  x <- many space *> number
  y <- many space *> number
  z <- many space *> number
  _ <- newline
  pure $ Tri x y z

columnParser :: Parser [Tri]
columnParser = fmap mconcat <$> many1 $ do
  x <- many space *> number
  x' <- many space *> number
  x'' <- many space *> number
  _ <- newline
  y <- many space *> number
  y' <- many space *> number
  y'' <- many space *> number
  _ <- newline
  z <- many space *> number
  z' <- many space *> number
  z'' <- many space *> number
  _ <- newline
  pure [Tri x y z, Tri x' y' z', Tri x'' y'' z'']

valid :: Tri -> Bool
valid (Tri x y z) = case [x, y, z] |> sort of
  [a, b, c] | a + b > c -> True
  _ -> False

-- 917
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filter valid
    |> length

-- 1649
solution2 :: Input -> Int
solution2 input =
  parseOrDie columnParser input
    |> filter valid
    |> length

solution :: Solution
solution = PureSolution solution1 917 solution2 1649
