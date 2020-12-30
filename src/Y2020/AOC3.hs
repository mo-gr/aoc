module Y2020.AOC3 where

import           Data.Either            (fromRight)
import           Text.Parsec            (many1, skipMany, space, string, (<|>))
import           Text.Parsec.ByteString (Parser, parseFromFile)

data World = Tree | Open deriving (Show)

type Line = [World]

treeParser :: Parser World
treeParser = string "#" >> return Tree
openParser :: Parser World
openParser = string "." >> return Open

lineParser :: Parser Line
lineParser = do
  line <- many1 (treeParser <|> openParser) <* skipMany space
  return $ cycle line

inputParser :: Parser [Line]
inputParser = many1 lineParser

exampleInput :: [Line]
exampleInput = [
  cycle [Open, Open, Tree, Tree],
  cycle [Open, Tree]
  ]

countTrees :: [Line] -> Int -> Int
countTrees [] _ = 0
countTrees (line:more) offset = case line !! (offset + 3) of
  Tree -> 1 + countTrees more (offset + 3)
  Open -> 0 + countTrees more (offset + 3)

countTreesNM :: Int -> Int -> [Line] -> Int -> Int
countTreesNM _ _ [] _ = 0
countTreesNM n m currentLines offset = case head currentLines !! (offset + n) of
    Tree -> 1 + countTreesNM n m (drop m currentLines) (offset + n)
    Open -> 0 + countTreesNM n m (drop m currentLines) (offset + n)

-- 164
solution1 :: IO Int
solution1 = do
  world <- fromRight [] <$> parseFromFile inputParser "AOC3.input"
  return $ countTrees (tail world) 0

-- 5007658656
solution2 :: IO Int
solution2 = do
  world <- fromRight [] <$> parseFromFile inputParser "AOC3.input"
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  return . product $ (\(n,m) -> countTreesNM n m (drop m world) 0) <$> slopes
