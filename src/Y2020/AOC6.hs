module Y2020.AOC6 where

import           Text.Parsec            (endOfLine, many1, oneOf, sepBy)
import           Data.Either            (fromRight)
import qualified Data.Set               as S
import           Text.Parsec.ByteString (Parser, parseFromFile)

type Answer = Char
type Person = [Answer]
type Group = [Person]

answerParser :: Parser Answer
answerParser = oneOf ['a'..'z']

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
solution1 :: IO Int
solution1 = do
  groups <- fromRight [] <$> parseFromFile inputParser "AOC6.input"
  return . sum $ length . groupAnswers <$> groups

-- 3402
solution2 :: IO Int
solution2 = do
  groups <- fromRight [] <$> parseFromFile inputParser "AOC6.input"
  return . sum $ length . groupAnswers' <$> groups
