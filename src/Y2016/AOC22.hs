{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2016.AOC22 (solution) where

import AOC (Solution (PureSolution))
import AStar (aStar)
import Control.Monad (guard)
import Data.List (sortOn)
import qualified Data.Set as S
import Debug.Trace (traceShow, traceShowId)
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

validMoves :: [Node] -> [(Node, Node)]
--validMoves ns = pairs ns |> filter nodesAreNeighbours
validMoves ns = findEmptyNode ns |> \empty -> (, empty) <$> filter (nodesAreNeighbours empty) ns

nodesAreNeighbours :: Node -> Node -> Bool
nodesAreNeighbours n n'
  | manhatten n n' == 1 = True
  | otherwise = False

applyMove :: [Node] -> (Node, Node) -> [Node]
applyMove [] _ = []
applyMove (n : ns) m@(from, _to) | pos n == pos from = n {used = 0, avail = size n} : applyMove ns m
applyMove (n : ns) m@(from, to) | pos n == pos to = n {used = used n + used from, avail = avail n - used from} : applyMove ns m
applyMove (n : ns) m = n : applyMove ns m

findTargetData :: [Node] -> Node
findTargetData = last . filter ((== 0) . snd . pos) . sortOn (fst . pos)

findEmptyNode :: [Node] -> Node
findEmptyNode = last . sortOn avail

updateTarget :: Node -> (Node, Node) -> Node
updateTarget t (from, to)
  | pos t == pos from = to
  | otherwise = t

bfsToGetTarget :: [Node] -> Int
bfsToGetTarget startNodes = go 0 S.empty [(startNodes, findTargetData startNodes)]
  where
    go :: Int -> S.Set ([Node], Node) -> [([Node], Node)] -> Int
    go _ _ [] = error "no path"
    go !n !pastStates !state
      | any (\(_, t) -> pos t == (0, 0)) state = n
      | otherwise =
        go (succ (traceShowId n)) (S.union pastStates (S.fromList state)) $ do
          (nodes, target) <- state
          move <- validMoves nodes
          let afterMove = applyMove nodes move
          let newTarget = updateTarget target move
          guard $ S.notMember (afterMove, newTarget) pastStates
          [(afterMove, newTarget)]

originIsEmpty :: [Node] -> (Node, Node) -> Bool
originIsEmpty ns (f, _)
  | findEmptyNode ns == f = True
  | otherwise = False

aStarToTarget :: [Node] -> Int
aStarToTarget startNodes = case aStar
  (\(ns, t) -> S.fromList $ (\m -> (applyMove ns m, updateTarget t m)) <$> validMoves ns)
  (\_ _ -> 1)
  (\(ns, t) -> manhatten t (findEmptyNode ns))
  (\(ns, t) -> pos t == pos (findTargetData ns))
  (startNodes, findEmptyNode startNodes) of
  Just p -> length $ traceShow (fmap snd p) p
  _ -> error "no path"

aStarToTarget' :: [Node] -> Int
aStarToTarget' startNodes =
  let target = traceShowId $ findTargetData startNodes
   in case aStar
        (\ns -> S.fromList $ applyMove ns <$> validMoves ns)
        (\_ _ -> 1)
        (\ns -> manhatten (findEmptyNode ns) target)
        (\ns -> manhatten (findEmptyNode ns) target == 0)
        startNodes of
        Just p -> length $ traceShow (fmap findEmptyNode ([startNodes]<>p)) p
        _ -> error "no path"

manhatten00 :: Node -> Int
manhatten00 n = manhatten n (Node (0, 0) 0 0 0)

manhatten :: Node -> Node -> Int
manhatten n n' = (pos n, pos n') |> (\((x, y), (x', y')) -> abs (x - x') + abs (y - y'))

-- 987
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> pairs
    |> length

-- too low 219
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> \ns ->
      traceShow (findEmptyNode ns) ns
        |> \ns ->
          traceShow (findTargetData ns) ns
            |> \ns ->
              traceShow (manhatten (findTargetData ns) (findEmptyNode ns)) ns
                |> \ns ->
                  traceShow (manhatten00 (findTargetData ns)) ns
                    |> \ns ->
                      traceShow ((manhatten00 (findTargetData ns) * 5) + manhatten (findTargetData ns) (findEmptyNode ns)) ns
                        |> aStarToTarget

solution :: Solution
solution = PureSolution solution1 987 solution2 undefined

testData :: Input
testData = "root@ebhq-gridcenter# df -h\nFilesystem              Size  Used  Avail  Use%\n/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%\n"
