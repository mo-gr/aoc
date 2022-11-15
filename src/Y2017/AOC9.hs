{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC9 where

import AOC (Solution (PureSolution))
import Data.Functor (($>), (<&>))
import Data.Maybe (catMaybes)
import Text.Parsec (anyChar, char, choice, many, newline, noneOf, sepBy, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Group = Group [Group] | Garbage Int

instance Show Group where
  show (Garbage _) = ""
  show (Group []) = "{}"
  show (Group gs) = "{" <> concatMap show gs <> "}"

inputParser :: Parser Group
inputParser = groupParser <* newline

groupParser :: Parser Group
groupParser = do
  _ <- char '{'
  nested <- choice [groupParser, trashParser] `sepBy` char ','
  _ <- char '}'
  pure $ Group nested

trashParser :: Parser Group
trashParser = do
  _ <- char '<'
  g <-
    catMaybes
      <$> many (escapedParser <|> (innerTrashParser <&> Just))
  _ <- char '>'
  pure $ Garbage (length g)

escapedParser :: Parser (Maybe Char)
escapedParser = char '!' *> anyChar $> Nothing

innerTrashParser :: Parser Char
innerTrashParser = noneOf ">"

score :: Int -> Group -> Int
score _ (Garbage _) = 0
score base (Group []) = base
score base (Group gs) = base + sum (fmap (score (succ base)) gs)

garbage :: Group -> Int
garbage (Garbage g) = g
garbage (Group gs) = sum (fmap garbage gs)

-- 16827
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> score 1

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> garbage

-- 7298
solution :: Solution
solution = PureSolution solution1 16827 solution2 7298
