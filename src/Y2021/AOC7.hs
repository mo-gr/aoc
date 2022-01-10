module Y2021.AOC7 where

import AOC (Solution (PureSolution))
import Text.Parsec (char, sepBy1)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Position = Int

type Cost = Int

type CostFunction = Int -> Cost

costToAlignTo :: CostFunction -> [Position] -> Position -> Cost
costToAlignTo _ [] _ = 0
costToAlignTo cf (x : xs) pos = cf (abs (pos - x)) + costToAlignTo cf xs pos

idCost :: CostFunction
idCost n = n

fibCost :: CostFunction
fibCost 0 = 0
fibCost 1 = 1
fibCost n = fibCost (n - 1) + n

minToMax :: [Position] -> [Position]
minToMax xs = [minimum xs .. maximum xs]

calculateCheapestCost :: CostFunction -> [Position] -> Cost
calculateCheapestCost cf ps = minimum $ fmap (costToAlignTo cf ps) (minToMax ps)

inputParser :: Parser [Position]
inputParser = sepBy1 number $ char ','

-- 349357
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> calculateCheapestCost idCost

-- 96708205
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> calculateCheapestCost fibCost

solution :: Solution
solution = PureSolution solution1 349357 solution2 96708205
