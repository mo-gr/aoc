module Y2021.AOC8 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.List (delete, find)
import Data.Maybe (fromMaybe)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (count, many, many1, oneOf, space, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

type Line = ([String], [String])

wordParser :: Parser String
wordParser = many1 (oneOf "abcdefg") <* many space

lineParser :: Parser Line
lineParser = do
  input' <- count 10 wordParser
  _ <- string "| "
  output' <- count 4 wordParser
  pure (input', output')

inputParser :: Parser [Line]
inputParser = many1 lineParser

find1 :: Line -> Maybe String
find1 (i, o) = find ((== 2) . length) (i ++ o)

find4 :: Line -> Maybe String
find4 (i, o) = find ((== 4) . length) (i ++ o)

find7 :: Line -> Maybe String
find7 (i, o) = find ((== 3) . length) (i ++ o)

is235 :: Line -> String -> Int
is235 l n = fromMaybe 2 $ do
  one <- find1 l <|> find7 l
  if all (`elem` n) one
    then pure 3
    else do
      four <- find4 l
      let fourMinusOne = delete (last one) $ delete (head one) four
      if all (`elem` n) fourMinusOne then pure 5 else Nothing

is069 :: Line -> String -> Int
is069 l n = fromMaybe 6 $ do
  four <- find4 l
  if all (`elem` n) four
    then pure 9
    else do
      one <- find1 l <|> find7 l
      if all (`elem` n) one then pure 0 else Nothing

toNumber :: Line -> String -> Int
toNumber l st = case length st of
  2 -> 1
  3 -> 7
  4 -> 4
  5 -> is235 l st
  6 -> is069 l st
  7 -> 8
  _ -> error "something went wrong"

isSimpleNumber :: String -> Bool
isSimpleNumber s = case length s of
  2 -> True
  3 -> True
  4 -> True
  7 -> True
  _ -> False

countSimpleNumberInOutput :: [Line] -> Int
countSimpleNumberInOutput [] = 0
countSimpleNumberInOutput ((_, out) : ls) = countSimpleNumberInOutput ls + length (filter isSimpleNumber out)

output :: Line -> Int
output l@(_, out) = asInt $ toNumber l <$> out
  where
    asInt [a, b, c, d] = a * 1000 + b * 100 + c * 10 + d
    asInt _ = error "something went wrong"

-- 470
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> countSimpleNumberInOutput

-- 989396
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> fmap output
    |> sum

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 470 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 989396 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 solution2 verify
