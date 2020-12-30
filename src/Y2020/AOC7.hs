module Y2020.AOC7 where

import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec (many1, letter, space, string, (<|>), digit, sepBy, optional)
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe (fromJust)

type Color = String
data BagType = BagType {
  color::Color,
  mayContain :: [Color]
} deriving (Show)

number :: Parser Int
number = read <$> many1 digit

colorParser :: Parser Color
colorParser = do
  modifier <- many1 letter <* space
  col <- many1 letter
  return $ modifier ++ " " ++ col

contentParser :: Parser [Color]
contentParser = do
  num <- number <* space
  col <- colorParser <* space
  _ <- string "bag" >> optional (string  "s")
  return $ replicate num col

bagTypeParser :: Parser BagType
bagTypeParser = do
  color' <- colorParser <* space
  _ <- string "bags contain" <* space
  contents <- (string "no other bags" >> return [])
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
solution1 :: IO Int
solution1 = do
  rules' <- fromRight [] <$> parseFromFile inputParser  "AOC7.input"
  return $ length . filter ("shiny gold" `elem`) $ recContent (rules rules') <$> (color <$> rules')

-- 5312
solution2 :: IO Int
solution2 = do
  rules' <- fromRight [] <$> parseFromFile inputParser  "AOC7.input"
  return $ countBags (rulesWithCount rules') "shiny gold"
