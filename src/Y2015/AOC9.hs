module Y2015.AOC9 where

import Data.List (nub, permutations, sort)
import qualified Data.Map.Strict as M
import Test.HUnit (Test (TestCase, TestList), assertEqual)
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

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 207 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 804 . solution2 =<< input
    ]
