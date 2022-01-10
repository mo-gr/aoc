module Y2020.AOC7 where

import AOC (Solution (PureSolution))
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Text.Parsec (letter, many1, optional, sepBy, space, string, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie)

type Color = String

data BagType = BagType
  { color :: Color,
    mayContain :: [Color]
  }
  deriving (Show)

colorParser :: Parser Color
colorParser = do
  modifier <- many1 letter <* space
  col <- many1 letter
  return $ modifier ++ " " ++ col

contentParser :: Parser [Color]
contentParser = do
  num <- number <* space
  col <- colorParser <* space
  _ <- string "bag" >> optional (string "s")
  return $ replicate num col

bagTypeParser :: Parser BagType
bagTypeParser = do
  color' <- colorParser <* space
  _ <- string "bags contain" <* space
  contents <-
    (string "no other bags" >> return [])
      <|> (concat <$> (contentParser `sepBy` string ", "))
  _ <- string "."
  return $ BagType color' contents

inputParser :: Parser [BagType]
inputParser = many1 (bagTypeParser <* space)

type Rules = Map.Map Color [Color]

rules :: [BagType] -> Map.Map Color [Color]
rules bags = Map.fromList $ (\b -> (color b, nub $ mayContain b)) <$> bags

rulesWithCount :: [BagType] -> Map.Map Color [Color]
rulesWithCount bags = Map.fromList $ (\b -> (color b, mayContain b)) <$> bags

contentByColor :: Rules -> Color -> [Color]
contentByColor rs c = fromJust $ Map.lookup c rs

recContent :: Rules -> Color -> [Color]
recContent rs c = contentByColor rs c ++ concat (recContent rs <$> contentByColor rs c)

countBags :: Rules -> Color -> Int
countBags rs c = length (contentByColor rs c) + sum (countBags rs <$> contentByColor rs c)

-- 355
solution1, solution2 :: Input -> Int
solution1 input =
  let rules' = parseOrDie inputParser input
   in length . filter ("shiny gold" `elem`) $ recContent (rules rules') <$> (color <$> rules')
-- 5312
solution2 input =
  let rules' = parseOrDie inputParser input
   in countBags (rulesWithCount rules') "shiny gold"

solution :: Solution
solution = PureSolution solution1 355 solution2 5312
