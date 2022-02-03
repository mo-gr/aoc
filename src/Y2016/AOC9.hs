module Y2016.AOC9 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.ByteString.Char8 (pack)
import Text.Parsec (anyChar, char, count, many1, noneOf, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

inputParser :: Parser String
inputParser = mconcat <$> many1 (try marker <|> content) -- <* newline

marker, content :: Parser String
marker = do
  _ <- char '('
  charCount <- number
  _ <- char 'x'
  repetitions <- number
  _ <- char ')'
  snip <- count charCount anyChar
  pure . mconcat $ replicate repetitions snip
content = many1 $ noneOf "(\n"

inputCountParser :: Parser Int
inputCountParser = sum <$> many1 (simpleCount <|> deepCount)
  where
    simpleCount = length <$> many1 (noneOf "(\n")
    deepCount = do
      _ <- char '('
      charCount <- number
      _ <- char 'x'
      repetitions <- number
      _ <- char ')'
      snip <- count charCount anyChar
      pure $ repetitions * parseOrDie inputCountParser (pack snip)

-- 97714
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> length

-- 10762972461
solution2 :: Input -> Int
solution2 = parseOrDie inputCountParser

solution :: Solution
solution = PureSolution solution1 97714 solution2 10762972461
