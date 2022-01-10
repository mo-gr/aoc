module Y2015.AOC9 where

import AOC (Solution (PureSolution))
import Data.List (nub, permutations, sort)
import qualified Data.Map.Strict as M
import Text.Parsec (letter, many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Distances = M.Map (String, String) Int

inputParser :: Parser Distances
inputParser = do
  distances <- many1 $ do
    from <- many1 letter
    _ <- string " to "
    to <- many1 letter
    _ <- string " = "
    dist <- number
    _ <- newline
    pure ((from, to), dist)
  pure . M.fromList $ distances <> fmap flipFirst distances
  where
    flipFirst ((a, b), c) = ((b, a), c)

pathCosts :: Distances -> [Int]
pathCosts d = do
  hops <- M.keys d |> fmap fst |> sort |> nub |> permutations
  let cost = zip hops (tail hops) |> fmap (d M.!)
  pure $ sum cost

-- 207
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> pathCosts
    |> minimum

-- 804
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> pathCosts
    |> maximum

solution :: Solution
solution = PureSolution solution1 207 solution2 804
