{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC14 where

import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), times)
import Text.Parsec (many1, upper, newline, count, string)
import Data.List (find, group, sort, minimum, maximum)

type Polymer = String
type Reaction = (String, Char)

polymerParser :: Parser Polymer
polymerParser = many1 upper <* newline

reactionParser :: Parser Reaction
reactionParser = do
  inp <- count 2 upper
  _ <- string " -> "
  out <- upper <* newline
  pure (inp, out)

inputParser :: Parser (Polymer, [Reaction])
inputParser = do
  p <- polymerParser <* newline
  rs <- many1 reactionParser
  pure (p, rs)

react :: Polymer -> [Reaction] -> Polymer
react [] rs = []
react (p:[]) rs = [p]
react (p:p':ps) rs = case find ((== [p,p']) . fst) rs of
  Just (_, i) -> p:i:react (p':ps) rs
  Nothing -> react (p':ps) rs

reactTimes :: Int -> Polymer -> [Reaction] -> Polymer
reactTimes n p rs = times n (flip react rs) p

score :: Polymer -> Int
score p = sort p |> group |> fmap length|> \l -> maximum l - minimum l

-- 2712
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> uncurry (reactTimes 10)
    |> score

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
      |> uncurry (reactTimes 40)
      |> score

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 2712 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" undefined . solution2 =<< input
    ]

testData :: Input
testData = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C\n"