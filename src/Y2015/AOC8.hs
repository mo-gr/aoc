module Y2015.AOC8 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (char, hexDigit, lower, many, many1, newline, notFollowedBy, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

type SantaString = String

type RawString = String

santaParser :: Parser [SantaString]
santaParser = many1 $ do
  _ <- char '"'
  cs <-
    many $
      try (string "\\\"" $> '"' <* notFollowedBy newline)
        <|> try (string "\\x" *> hexDigit *> hexDigit $> 'H')
        <|> try (char '\\' *> char '\\' $> '\\')
        <|> lower
  _ <- char '"'
  _ <- newline
  pure cs

escapingParser :: Parser [SantaString]
escapingParser = many1 $ do
  s <- char '"' $> "\"\\\""
  cs <-
    many $
      try (char '\\' $> "\\\\")
        <|> try (char '"' $> "\\\"" <* notFollowedBy newline)
        <|> (lower >>= \c -> pure [c])
        <|> (hexDigit >>= \c -> pure [c])
  e <- char '"' $> "\\\"\""
  _ <- newline
  pure $ s <> mconcat cs <> e

rawParser :: Parser [RawString]
rawParser = many1 (many1 (lower <|> hexDigit <|> char '"' <|> char '\\') <* newline)

-- 1333
solution1 :: Input -> Int
solution1 input =
  let santaLength =
        parseOrDie santaParser input
          |> fmap length
          |> sum
      rawLength =
        parseOrDie rawParser input
          |> fmap length
          |> sum
   in rawLength - santaLength

-- 2046
solution2 :: Input -> Int
solution2 input =
  let escapedLength =
        parseOrDie escapingParser input
          |> fmap length
          |> sum
      rawLength =
        parseOrDie rawParser input
          |> fmap length
          |> sum
   in escapedLength - rawLength

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 1333 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 2046 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 solution2 verify
