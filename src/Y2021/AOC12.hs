{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC12 where

import Data.Functor (($>), (<&>))
import Data.List (notElem, sort, group)
import Debug.Trace
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (lower, many1, newline, string, try, upper, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Cave = Start | End | Small String | Big String deriving (Show, Eq, Ord)

type Connection = (Cave, Cave)

type Path = [Cave]

caveParser :: Parser Cave
caveParser =
  try (string "start" $> Start)
    <|> try (string "end" $> End)
    <|> (many1 upper <&> Big)
    <|> (many1 lower <&> Small)

inputParser :: Parser [Connection]
inputParser = many1 $ do
  c1 <- caveParser <* string "-"
  c2 <- caveParser <* newline
  pure (c1, c2)

candidates :: [Connection] -> Cave -> [Cave]
candidates cs cave = mconcat $ fmap cf cs
  where
    cf (c, c') | c == cave = [c']
    cf (c, c') | c' == cave = [c]
    cf _ = []

type Filter = Path -> Cave -> Bool

allPaths :: Filter -> Path -> [Connection] -> [Path]
allPaths _ p cs | length p > 30 = error $ "too deep: " ++ show p
allPaths _ (End : p) _ = [(End : p)]
allPaths fInvalid path@(c : _) cs = mconcat $ do
  candidate <- filter (fInvalid path) $ candidates cs c
  pure $ allPaths fInvalid (candidate : path) cs

invalid :: Path -> Cave -> Bool
invalid _ (Big _) = True
invalid path c = (c `notElem` path)

hasBeenToASmallTwice :: Path -> Bool
hasBeenToASmallTwice p = filter onlySmall p
  |> sort
  |> group
  |> fmap length
  |> any (> 1)
  where onlySmall (Small _) = True
        onlySmall _ = False

invalid' :: Path -> Cave -> Bool
invalid' _ (Big _) = True
invalid' path c@(Small _) | hasBeenToASmallTwice path = (c `notElem` path)
invalid' path (Small _) = True
invalid' path c = (c `notElem` path)

-- 5254
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> allPaths invalid [Start]
    |> length

-- 149385
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> allPaths invalid' [Start]
    |> length

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 5254 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 149385 . solution2 =<< input
    ]

testData :: Input
testData = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n"
