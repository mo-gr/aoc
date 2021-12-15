{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC15 where
import Data.Char (digitToInt)

import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import Text.Parsec (many1, digit, newline)
import Data.List (sort, sum, minimumBy)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Risk = Int

type Cave = [[Risk]]

type Point = (Int, Int)
type Path = [Point]

risk :: Parser [Risk]
risk = fmap digitToInt <$> many1 digit

inputParser :: Parser Cave
inputParser = many1 (risk <* newline)

paths :: Cave -> (Point, Point) -> Path -> [Path]
paths c (ps, pe) path | ps == pe = [pe:path]
paths c ((sx, sy), (ex, ey)) path =
  [(sx + 1, sy), (sx, sy + 1)]
    |> filter (\(x,y) -> x <= ex && y <= ey)
    |> fmap (\p -> paths c (p, (ex, ey)) ((sx,sy):path))
    |> mconcat

runPaths :: Cave -> [Path]
runPaths c = paths c ((0,0), ((length $ head c) - 1, length c - 1)) []

runAStar :: Cave -> Path
runAStar c = let start = (0,0)
                 end =  ((length $ head c) - 1, length c - 1)
             in aStar c start end (getAt c)

runAStarCost :: Cave -> Int
runAStarCost c = runAStar c |> fmap (getAt c) |> reverse |> drop 1 |> sum

getAt :: Cave -> Point -> Risk
getAt c (x, y) = (c !! y) !! x

findCheapest :: Cave -> Int
findCheapest c = runPaths c
  |> (fmap.fmap) (getAt c)
  |> fmap sum
  |> sort
  |> head
  |> (flip (-) (head $ head c))

infinity :: Risk
infinity = 100000

neighbours :: Point -> Cave -> [Point]
neighbours (x, y) c = filter bounds
  [(x - 1, y), (x + 1, y), (x, y -1), (x, y + 1)]
  where
    mx = (length $ head c) - 1
    my = length c - 1
    bounds (x', _) | x' < 0 || x' > mx = False
    bounds (_, y') | y' < 0 || y' > my = False
    bounds (x', y') | x == x' && y == y' = False
    bounds _ = True

reconstruct :: M.Map Point Point -> Point -> Path
reconstruct from curr = case M.lookup curr from of
  Just prev -> curr : reconstruct from prev
  Nothing -> [curr]

inserts :: (Ord k) => M.Map k v -> v -> [k] -> M.Map k v
inserts m v = foldl (\ m' k -> M.insert k v m') m

aStar :: Cave -> Point -> Point -> (Point -> Risk) -> Path
aStar c start end costFn =
  let baseOpenSet = S.singleton start
      baseFrom = M.empty
      baseCost = M.fromList [(start, 0)]
      recur :: S.Set Point -> M.Map Point Point -> M.Map Point Risk -> Path
      recur s _f _c  | s == S.empty = error "no path"
      recur openSet from cost = let curr = minimumBy (\a b -> compare (M.findWithDefault infinity a cost) (M.findWithDefault infinity b cost)) $ S.toList openSet
                 in if curr == end then reconstruct from curr
                    else let openSet' = S.delete curr openSet
                             ns = neighbours curr c
                             tentativeScores = zip ns $ (\n ->costFn n +  M.findWithDefault infinity curr cost) <$> ns
                         in tentativeScores
                            |> filter (\(n, ts) -> ts < M.findWithDefault infinity n cost)
                            |> \prs -> recur (S.fromList (fst <$> prs) `S.union` openSet')
                                             (inserts from curr (fst <$> prs))
                                             (foldl (\m (n, ts) -> M.insert n ts m) cost prs)
  in recur baseOpenSet baseFrom baseCost

inc :: Risk -> Risk
inc 9 = 1
inc n = n+1

fiveFold :: [Risk] -> [Risk]
fiveFold r = r 
  ++ (inc <$> r)
  ++ (inc.inc <$> r)
  ++ (inc.inc.inc <$> r)
  ++ (inc.inc.inc.inc <$> r)

blowUp :: Cave -> Cave
blowUp c = (fiveFold <$> c )
  ++ (fmap inc <$> (fiveFold <$> c ))
  ++ (fmap (inc.inc) <$> (fiveFold <$> c ))
  ++ (fmap (inc.inc.inc) <$> (fiveFold <$> c ))
  ++ (fmap (inc.inc.inc.inc) <$> (fiveFold <$> c ))

-- 388
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> runAStarCost

-- 2819
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> blowUp
    |> runAStarCost

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 388 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 2819 . solution2 =<< input
    ]

testData :: Input
testData = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581\n"