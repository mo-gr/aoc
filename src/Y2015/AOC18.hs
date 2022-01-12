module Y2015.AOC18 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Text.Parsec (char, many, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, times, (|>))

type Point = (Int, Int)

type World = M.Map Point Cell

data Cell = Alive | Dead deriving (Eq)

instance Show Cell where
  show Alive = "#"
  show Dead = "."

inputParser :: Parser World
inputParser = do
  listOfCells <- many1 (cellParser <* many newline)
  pure $ M.fromList (kvPairs listOfCells)
  where
    kvPairs :: [[Cell]] -> [(Point, Cell)]
    kvPairs os = do
      x <- [0 .. length (head os) - 1]
      y <- [0 .. length os - 1]
      pure ((x, y), (os !! y) !! x)
    cellParser = many1 $ (char '#' $> Alive) <|> (char '.' $> Dead)

neighbours :: Point -> [Point]
neighbours (x, y) = filter (/= (x, y)) $ do
  y' <- (+ y) <$> [-1 .. 1]
  x' <- (+ x) <$> [-1 .. 1]
  guard $ x' >= 0 && x' < 100
  guard $ y' >= 0 && y' < 100
  pure (x', y')

evolve :: World -> World
evolve w = M.foldlWithKey ev w w
  where
    ev :: World -> Point -> Cell -> World
    ev acc p Alive = case neighbours p |> fmap (`M.lookup` w) |> countAlive of
      2 -> acc
      3 -> acc
      _ -> M.insert p Dead acc
    ev acc p Dead = case neighbours p |> fmap (`M.lookup` w) |> countAlive of
      3 -> M.insert p Alive acc
      _ -> acc
    countAlive = length . filter (== Just Alive)

evolve' :: World -> World
evolve' w = M.foldlWithKey ev w w
  where
    ev :: World -> Point -> Cell -> World
    ev acc p _ | p `elem` stuck = M.insert p Alive acc
    ev acc p Alive = case neighbours p |> fmap stuckLookup |> countAlive of
      2 -> acc
      3 -> acc
      _ -> M.insert p Dead acc
    ev acc p Dead = case neighbours p |> fmap stuckLookup |> countAlive of
      3 -> M.insert p Alive acc
      _ -> acc
    countAlive = length . filter (== Just Alive)
    stuckLookup p | p `elem` stuck = Just Alive
    stuckLookup p = M.lookup p w
    stuck = [(0, 0), (0, 99), (99, 0), (99, 99)]

-- 814
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> times 100 evolve
    |> M.elems
    |> filter (== Alive)
    |> length

-- 924
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> times 100 evolve'
    |> M.elems
    |> filter (== Alive)
    |> length

solution :: Solution
solution = PureSolution solution1 814 solution2 924
