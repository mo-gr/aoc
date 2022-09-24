{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC22 (solution) where

import AOC (Solution (PureSolution))
import AStar (aStar)
import Control.Monad (guard)
import Data.List (sortOn)
import qualified Data.Set as S
import Text.Parsec (many1, newline, space, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

type Point = (Int, Int)

data Node = Node {pos :: Point, size :: Int, used :: Int, avail :: Int}
  deriving (Eq, Ord)

instance Show Node where
  show n = show (pos n) -- <> " " <> show (used n) <> "/" <> show (size n)

inputParser :: Parser [Node]
inputParser = do
  _call <- string "root@ebhq-gridcenter# df -h" <* newline
  _header <- string "Filesystem              Size  Used  Avail  Use%" <* newline
  many1 $ do
    x <- string "/dev/grid/node-x" *> number
    y <- string "-y" *> number
    sz <- many1 space *> number <* string "T"
    us <- many1 space *> number <* string "T"
    av <- many1 space *> number <* string "T"
    _px <- many1 space *> number <* string "%" <* newline
    pure $ Node (x, y) sz us av

pairs :: [Node] -> [(Node, Node)]
pairs ns = do
  a <- ns
  b <- ns
  guard $ a /= b
  guard $ used a > 0
  guard $ used a < avail b
  pure (a, b)

nodesAreNeighbours :: Point -> Point -> Bool
nodesAreNeighbours n n'
  | manhatten n n' == 1 = True
  | otherwise = False

findTargetData :: [Node] -> Node
findTargetData = last . filter ((== 0) . snd . pos) . sortOn (fst . pos)

findEmptyNode :: [Node] -> Node
findEmptyNode = last . sortOn avail

aStarFromTo :: [Point] -> Point -> Point -> Int
aStarFromTo nodes from target =
  let graph n = filter (nodesAreNeighbours n) nodes |> S.fromList
      distance _ _ = 1
      heuristic n = manhatten n target
      goal n = target == n
   in case aStar graph distance heuristic goal from of
        Just p -> length p
        _ -> error "no path"

manhatten :: Point -> Point -> Int
manhatten (x, y) (x', y') = abs (x - x') + abs (y - y')

validPos :: [Node] -> [Point]
validPos nodes = filter ((< 100) . size) nodes |> fmap pos

oneBefore :: Point -> Point
oneBefore (x, y) = (pred x, y)

solvePuzzle :: [Node] -> Int
solvePuzzle nodes =
  let emptyNodePos = findEmptyNode nodes |> pos
      targetPos = findTargetData nodes |> pos
      costToMoveEmptyNextToGoal = aStarFromTo (validPos nodes) emptyNodePos (oneBefore targetPos)
      costToMoveGoalByOne = 5
      distanceToMoveGoal = aStarFromTo (validPos nodes) targetPos (1, 0)
      finalMoveCost = 1
   in costToMoveEmptyNextToGoal + (costToMoveGoalByOne * distanceToMoveGoal) + finalMoveCost

-- 987
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> pairs
    |> length

-- 220
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solvePuzzle

solution :: Solution
solution = PureSolution solution1 987 solution2 220
