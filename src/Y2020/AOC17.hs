module Y2020.AOC17 where

import AOC (Solution (PureSolution))
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Util (Input)

type Coord = (Int, Int, Int)

type HyperCoord = (Int, Int, Int, Int)

type Cube = Coord

type HyperCube = HyperCoord

type Cubes = S.Set Cube

type HyperCubes = S.Set HyperCube

hyper :: Cube -> HyperCube
hyper (x, y, z) = (x, y, z, 0)

neighbours :: Coord -> Cubes
neighbours (x, y, z) = S.fromList $
  filter (/= (x, y, z)) $ do
    x' <- enumFromTo (x -1) (x + 1)
    y' <- enumFromTo (y -1) (y + 1)
    z' <- enumFromTo (z -1) (z + 1)
    return (x', y', z')

neighbours' :: HyperCoord -> HyperCubes
neighbours' (x, y, z, h) = S.fromList $
  filter (/= (x, y, z, h)) $ do
    x' <- enumFromTo (x -1) (x + 1)
    y' <- enumFromTo (y -1) (y + 1)
    z' <- enumFromTo (z -1) (z + 1)
    h' <- enumFromTo (h -1) (h + 1)
    return (x', y', z', h')

relevantCubes :: Cubes -> Cubes
relevantCubes = foldl (\s c -> S.union s $ neighbours c) S.empty

relevantCubes' :: HyperCubes -> HyperCubes
relevantCubes' = foldl (\s c -> S.union s $ neighbours' c) S.empty

tickAlive :: Cubes -> Cube -> Maybe Cube
tickAlive alive c =
  let relevantNeighbours = neighbours c
   in if length (S.intersection alive relevantNeighbours) `elem` [2, 3] then Just c else Nothing

tickAlive' :: HyperCubes -> HyperCube -> Maybe HyperCube
tickAlive' alive c =
  let relevantNeighbours = neighbours' c
   in if length (S.intersection alive relevantNeighbours) `elem` [2, 3] then Just c else Nothing

tickDead :: Cubes -> Cube -> Maybe Cube
tickDead alive c =
  let relevantNeighbours = neighbours c
   in if length (S.intersection alive relevantNeighbours) == 3 then Just c else Nothing

tickDead' :: HyperCubes -> HyperCube -> Maybe HyperCube
tickDead' alive c =
  let relevantNeighbours = neighbours' c
   in if length (S.intersection alive relevantNeighbours) == 3 then Just c else Nothing

cycleAlive :: Cubes -> Cubes
cycleAlive alive =
  let relevant = relevantCubes alive
      dead = S.difference relevant alive
      stillAlive = S.map (tickAlive alive) alive
      born = S.map (tickDead alive) dead
   in S.fromList $ catMaybes (S.toList stillAlive ++ S.toList born)

cycleAlive' :: HyperCubes -> HyperCubes
cycleAlive' alive =
  let relevant = relevantCubes' alive
      dead = S.difference relevant alive
      stillAlive = S.map (tickAlive' alive) alive
      born = S.map (tickDead' alive) dead
   in S.fromList $ catMaybes (S.toList stillAlive ++ S.toList born)

reapply :: Int -> (a -> a) -> a -> a
reapply 1 f a = f a
reapply n f a = reapply (n - 1) f (f a)

-- .#.
-- ..#
-- ###
example :: Cubes
example = S.fromList [(0, -1, 0), (1, 0, 0), (-1, 1, 0), (0, 1, 0), (1, 1, 0)]

-- 12345678
-- ######.# 0
-- ##.###.# 1
-- #.###.## 2
-- ..#..### 3
-- ##.#.#.# 4
-- ##...##. 5
-- #.#.##.# 6
-- .###.### 7
input :: Cubes
input =
  S.fromList
    [ (1, 0, 0),
      (2, 0, 0),
      (3, 0, 0),
      (4, 0, 0),
      (5, 0, 0),
      (6, 0, 0),
      (8, 0, 0),
      (1, 1, 0),
      (2, 1, 0),
      (4, 1, 0),
      (5, 1, 0),
      (6, 1, 0),
      (8, 1, 0),
      (1, 2, 0),
      (3, 2, 0),
      (4, 2, 0),
      (5, 2, 0),
      (7, 2, 0),
      (8, 2, 0),
      (3, 3, 0),
      (6, 3, 0),
      (7, 3, 0),
      (8, 3, 0),
      (1, 4, 0),
      (2, 4, 0),
      (4, 4, 0),
      (6, 4, 0),
      (8, 4, 0),
      (1, 5, 0),
      (2, 5, 0),
      (6, 5, 0),
      (7, 5, 0),
      (1, 6, 0),
      (3, 6, 0),
      (5, 6, 0),
      (6, 6, 0),
      (8, 6, 0),
      (2, 7, 0),
      (3, 7, 0),
      (4, 7, 0),
      (6, 7, 0),
      (7, 7, 0),
      (8, 7, 0)
    ]

-- 348
solution1 :: Input -> Int
solution1 _input = length $ reapply 6 cycleAlive input

-- 2236
solution2 :: Input -> Int
solution2 _input = length $ reapply 6 cycleAlive' (S.map hyper input)

solution :: Solution
solution = PureSolution solution1 348 solution2 2236
