{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC9 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import Text.Parsec (newline, char, many, (<|>), choice, noneOf, sepBy, anyChar)
import Data.Functor (($>))
import Data.Maybe (catMaybes)

newtype Group = Group [Group]

instance Show Group where
  show (Group []) = "{}"
  show (Group gs) = "{" <> concatMap show gs  <> "}"

inputParser :: Parser Group
inputParser = groupParser <* newline

groupParser :: Parser Group
groupParser = do
  _ <- char '{'
  nested <- nestedGroupsParser
  _ <- char '}'
  pure $ Group nested

nestedGroupsParser :: Parser [Group]
nestedGroupsParser = do
  catMaybes <$> sepBy (choice [Just <$> groupParser, trashParser $> Nothing]) (char ',')

trashParser :: Parser ()
trashParser = do
  _ <- char '<'
  _ <- many $ (char '!' *> anyChar) <|> noneOf ">"
  _ <- char '>'
  pure ()


score :: Int ->  Group -> Int
score base (Group []) = base
score base (Group gs) = base + sum (fmap (score (succ base)) gs) 

-- 16827
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> score 1

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> undefined

solution :: Solution
solution = PureSolution solution1 16827 solution2 undefined

testData :: Input
testData = "{{<!!>},{<!!>},{<!!>},{<!!>}}\n"
--testData = "{{<!>},{<!>},{<!>},{<a>}}\n"