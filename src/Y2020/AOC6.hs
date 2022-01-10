module Y2020.AOC6 where

import AOC (Solution (PureSolution))
import qualified Data.Set as S
import Text.Parsec (endOfLine, many1, oneOf, sepBy)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

type Answer = Char

type Person = [Answer]

type Group = [Person]

answerParser :: Parser Answer
answerParser = oneOf ['a' .. 'z']

personParser :: Parser Person
personParser = many1 answerParser <* endOfLine

groupParser :: Parser Group
groupParser = many1 personParser

inputParser :: Parser [Group]
inputParser = groupParser `sepBy` endOfLine

groupAnswers :: Group -> S.Set Answer
groupAnswers group = S.unions $ S.fromList <$> group

groupAnswers' :: Group -> S.Set Answer
groupAnswers' group = foldl1 S.intersection $ S.fromList <$> group

-- 6534
solution1, solution2 :: Input -> Int
solution1 input =
  parseOrDie inputParser input |> fmap (length . groupAnswers) |> sum
-- 3402
solution2 input = parseOrDie inputParser input |> fmap (length . groupAnswers') |> sum

solution :: Solution
solution = PureSolution solution1 6534 solution2 3402
