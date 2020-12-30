module Y2020.AOC1 where

import           Text.Parsec            (digit, many1, skipMany, space)
import           Text.Parsec.ByteString (Parser, parseFromFile)

import           Data.List              (find)

number :: Parser Integer
number = read <$> many1 digit

parseExpenses :: Parser [Integer]
parseExpenses = many1 (number <* skipMany space)

exampleInput :: [Integer]
exampleInput = [978,
                979,
                366,
                1721,
                299,
                675,
                1456]

mulExpenses :: Maybe (Integer, Integer) -> Maybe Integer
mulExpenses (Just (x,y)) = Just (x * y)
mulExpenses _            = Nothing

mulExpenses3 :: Maybe (Integer, Integer, Integer) -> Maybe Integer
mulExpenses3 (Just (x, y, z)) = Just (x * y * z)
mulExpenses3 _                = Nothing

allPairs :: [a] -> [(a, a)]
allPairs ls @ (_ : xs) = [(x, pair) | x <- ls, pair <- xs]
allPairs _             = []

allTriples :: [a] -> [(a, a, a)]
allTriples ls @ (_ : xs @ (_ : ys)) = [(x, pair, triple) | x <- ls, pair <- xs, triple <- ys]
allTriples _ = []

findSum2020 :: [Integer] -> Maybe (Integer, Integer)
findSum2020 = find (\(x, y) -> x + y == 2020) . allPairs

findSum20203 :: [Integer] -> Maybe (Integer, Integer, Integer)
findSum20203 = find (\(x, y, z) -> x + y + z == 2020) . allTriples

-- 545379
solution1 :: IO (Maybe Integer)
solution1 = do
  expensesOrErr <- parseFromFile parseExpenses "AOC1.input"
  case expensesOrErr of
    Left e         -> error $ show e
    Right expenses -> return . mulExpenses $ findSum2020 expenses

-- 257778836
solution2 :: IO (Maybe Integer)
solution2 = do
  expensesOrErr <- parseFromFile parseExpenses "AOC1.input"
  case expensesOrErr of
    Left e         -> error $ show e
    Right expenses -> return . mulExpenses3 $ findSum20203 expenses
