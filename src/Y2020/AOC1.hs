module Y2020.AOC1 where

import           Text.Parsec            (many1, skipMany, space)
import           Text.Parsec.ByteString (Parser, parseFromFile)
import           Data.List              (find)
import           Util                   (number)

parseExpenses :: Parser [Int]
parseExpenses = many1 (number <* skipMany space)

exampleInput :: [Int]
exampleInput = [978,
                979,
                366,
                1721,
                299,
                675,
                1456]

mulExpenses :: Maybe (Int, Int) -> Maybe Int
mulExpenses (Just (x,y)) = Just (x * y)
mulExpenses _            = Nothing

mulExpenses3 :: Maybe (Int, Int, Int) -> Maybe Int
mulExpenses3 (Just (x, y, z)) = Just (x * y * z)
mulExpenses3 _                = Nothing

allPairs :: [a] -> [(a, a)]
allPairs ls @ (_ : xs) = [(x, pair) | x <- ls, pair <- xs]
allPairs _             = []

allTriples :: [a] -> [(a, a, a)]
allTriples ls @ (_ : xs @ (_ : ys)) = [(x, pair, triple) | x <- ls, pair <- xs, triple <- ys]
allTriples _ = []

findSum2020 :: [Int] -> Maybe (Int, Int)
findSum2020 = find (\(x, y) -> x + y == 2020) . allPairs

findSum20203 :: [Int] -> Maybe (Int, Int, Int)
findSum20203 = find (\(x, y, z) -> x + y + z == 2020) . allTriples

-- 545379
solution1 :: IO (Maybe Int)
solution1 = do
  expensesOrErr <- parseFromFile parseExpenses "AOC1.input"
  case expensesOrErr of
    Left e         -> error $ show e
    Right expenses -> return . mulExpenses $ findSum2020 expenses

-- 257778836
solution2 :: IO (Maybe Int)
solution2 = do
  expensesOrErr <- parseFromFile parseExpenses "AOC1.input"
  case expensesOrErr of
    Left e         -> error $ show e
    Right expenses -> return . mulExpenses3 $ findSum20203 expenses
