{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC23 where

import Data.List (delete)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec.ByteString (Parser)
import Util (Input, (|>))

--  01234567891
-- #############
-- #...........# 0
-- ###B#C#B#D### 1
--   #A#D#C#A#   2
--   #########

-- Part 2
--  01234567891
-- #############
-- #...........# 0
-- ###B#C#B#D### 1
--   #D#C#B#A#   2
--   #D#B#A#C#   3
--   #A#D#C#A#   4
--   #########

type Point = (Int, Int)

data Pod = Amber | Bronze | Copper | Desert
  deriving (Show, Eq, Ord)

type World = (Int, M.Map Point Pod)

validPositions :: [Point]
validPositions =
  (2, 1) :
  (2, 2) :
  (4, 1) :
  (4, 2) :
  (6, 1) :
  (6, 2) :
  (8, 1) :
  (8, 2) : do
    x <- [0 .. 10]
    pure (x, 0)

validPositions2 :: [Point]
validPositions2 =
  (2, 1) :
  (2, 2) :
  (2, 3) :
  (2, 4) :
  (4, 1) :
  (4, 2) :
  (4, 3) :
  (4, 4) :
  (6, 1) :
  (6, 2) :
  (6, 3) :
  (6, 4) :
  (8, 1) :
  (8, 2) :
  (8, 3) :
  (8, 4) : do
    x <- [0 .. 10]
    pure (x, 0)

validTargets :: [Point]
validTargets = foldl (flip delete) validPositions [(2, 0), (4, 0), (6, 0), (8, 0)]

validTargets2 :: [Point]
validTargets2 = foldl (flip delete) validPositions2 [(2, 0), (4, 0), (6, 0), (8, 0)]

isPossible :: M.Map Point Pod -> Point -> Pod -> Point -> Bool
isPossible _m fr _ to | fr == to = False -- can't move in place
isPossible _m (_xf, 0) _p (_xt, 0) = False -- can't go from hallway to hallway
isPossible _m _fr Amber (xt, yt) | yt /= 0 && xt /= 2 = False -- amber can only go into first room
isPossible _m _fr Bronze (xt, yt) | yt /= 0 && xt /= 4 = False -- bronze can only go into second room
isPossible _m _fr Copper (xt, yt) | yt /= 0 && xt /= 6 = False -- copper can only go into third room
isPossible _m _fr Desert (xt, yt) | yt /= 0 && xt /= 8 = False -- desert can only go into fourth room
isPossible m _fr _p (xt, 1) | M.notMember (xt, 2) m = False -- has to go into room all the way
isPossible m _fr p (xt, 1) | M.lookup (xt, 2) m /= Just p = False -- can't go into room that has wrong pod
isPossible _m (2, 2) Amber _ = False -- happy amber never moves
isPossible _m (4, 2) Bronze _ = False -- happy bronze never moves
isPossible _m (6, 2) Copper _ = False -- happy copper never moves
isPossible _m (8, 2) Desert _ = False -- happy desert never moves
isPossible m (2, 1) Amber _ | M.lookup (2, 2) m == Just Amber = False -- happy amber never moves
isPossible m (4, 1) Bronze _ | M.lookup (4, 2) m == Just Bronze = False -- happy bronze never moves
isPossible m (6, 1) Copper _ | M.lookup (6, 2) m == Just Copper = False -- happy copper never moves
isPossible m (8, 1) Desert _ | M.lookup (8, 2) m == Just Desert = False -- happy desert never moves
isPossible m _ _ t | M.member t m = False -- can only go to free fields
isPossible m (xf, 2) _ _to | M.member (xf, 1) m = False -- Can only leave room if no one is in the way
isPossible _m (xf, 2) _ (yf, 1) | xf == yf = False -- has to leave the room
isPossible m fr@(xf, _) _ (xt, _) | any (\x -> M.member (x, 0) (M.delete fr m)) [(min xf xt) .. (max xf xt)] = False -- path has to be free
isPossible m _ Amber (_, 0) | M.notMember (2, 2) m = False -- Amber has to go home directly if possible
isPossible m _ Amber (_, 0) | M.notMember (2, 1) m && M.lookup (2, 2) m == Just Amber = False -- Amber has to go home directly if possible
isPossible m _ Bronze (_, 0) | M.notMember (4, 2) m = False -- Bronze has to go home directly if possible
isPossible m _ Bronze (_, 0) | M.notMember (4, 1) m && M.lookup (4, 2) m == Just Bronze = False -- Bronze has to go home directly if possible
isPossible m _ Copper (_, 0) | M.notMember (6, 2) m = False -- Copper has to go home directly if possible
isPossible m _ Copper (_, 0) | M.notMember (6, 1) m && M.lookup (6, 2) m == Just Copper = False -- Copper has to go home directly if possible
isPossible m _ Desert (_, 0) | M.notMember (8, 2) m = False -- Desert has to go home directly if possible
isPossible m _ Desert (_, 0) | M.notMember (8, 1) m && M.lookup (8, 2) m == Just Desert = False -- Desert has to go home directly if possible
isPossible _ _ _ _ = True

isPossible2 :: M.Map Point Pod -> Point -> Pod -> Point -> Bool
isPossible2 _m fr _ to | fr == to = False -- can't move in place
isPossible2 _m (_xf, 0) _p (_xt, 0) = False -- can't go from hallway to hallway
isPossible2 _m _fr Amber (xt, yt) | yt /= 0 && xt /= 2 = False -- amber can only go into first room
isPossible2 _m _fr Bronze (xt, yt) | yt /= 0 && xt /= 4 = False -- bronze can only go into second room
isPossible2 _m _fr Copper (xt, yt) | yt /= 0 && xt /= 6 = False -- copper can only go into third room
isPossible2 _m _fr Desert (xt, yt) | yt /= 0 && xt /= 8 = False -- desert can only go into fourth room
isPossible2 m _fr _p (xt, yt) | yt > 0 && any (\y -> M.notMember (xt, y) m) [(yt + 1) .. 4] = False -- has to go into room all the way
isPossible2 m _fr p (xt, yt) | yt > 0 && any (\y -> M.lookup (xt, y) m /= Just p) [(yt + 1) .. 4] = False -- can't go into room that has wrong pod
isPossible2 _m (2, 4) Amber _ = False -- happy amber never moves
isPossible2 _m (4, 4) Bronze _ = False -- happy bronze never moves
isPossible2 _m (6, 4) Copper _ = False -- happy copper never moves
isPossible2 _m (8, 4) Desert _ = False -- happy desert never moves
isPossible2 m (2, yt) Amber _ | all (\y -> M.lookup (2, y) m == Just Amber) [yt .. 4] = False -- happy amber never moves
isPossible2 m (4, yt) Bronze _ | all (\y -> M.lookup (4, y) m == Just Bronze) [yt .. 4] = False -- happy amber never moves
isPossible2 m (6, yt) Copper _ | all (\y -> M.lookup (6, y) m == Just Copper) [yt .. 4] = False -- happy amber never moves
isPossible2 m (8, yt) Desert _ | all (\y -> M.lookup (8, y) m == Just Desert) [yt .. 4] = False -- happy amber never moves
isPossible2 m _ _ t | M.member t m = False -- can only go to free fields
isPossible2 m (xf, yf) _ _to | yf /= 0 && any (\y -> M.member (xf, y) m) [1 .. (yf - 1)] = False -- Can only leave room if no one is in the way
isPossible2 _m (xf, _yf) _ (xt, _yt) | xf == xt = False -- has to leave the room
isPossible2 m fr@(xf, _) _ (xt, _) | any (\x -> M.member (x, 0) (M.delete fr m)) [(min xf xt) .. (max xf xt)] = False -- path has to be free
isPossible2 m _ Amber (_, 0) | all (\y -> isNothing (M.lookup (2, y) m) || M.lookup (2, y) m == Just Amber) [1 .. 4] = False -- Amber has to go home directly if possible
isPossible2 m _ Bronze (_, 0) | all (\y -> isNothing (M.lookup (4, y) m) || M.lookup (4, y) m == Just Bronze) [1 .. 4] = False -- Bronze has to go home directly if possible
isPossible2 m _ Copper (_, 0) | all (\y -> isNothing (M.lookup (6, y) m) || M.lookup (6, y) m == Just Copper) [1 .. 4] = False -- Copper has to go home directly if possible
isPossible2 m _ Desert (_, 0) | all (\y -> isNothing (M.lookup (8, y) m) || M.lookup (8, y) m == Just Desert) [1 .. 4] = False -- Desert has to go home directly if possible
isPossible2 _ _ _ _ = True

updateKey :: Ord k => k -> k -> M.Map k a -> M.Map k a
updateKey k k1 m = case M.lookup k m of
  Nothing -> m
  Just a -> M.delete k m |> M.insert k1 a

move :: World -> (Point, Pod) -> Maybe [World]
move (en, wo) (fr, po) = case filter (isPossible wo fr po) validTargets of
  [] -> Nothing
  --  moves -> trace (show fr ++ show po ++ " " ++ show moves ++ "\n" ++ pretty (en, wo)) $ Just $ do
  moves -> Just $ do
    m <- moves
    pure (en + cost fr po m, updateKey fr m wo)

move2 :: World -> (Point, Pod) -> Maybe [World]
move2 (en, wo) (fr, po) = case filter (isPossible2 wo fr po) validTargets2 of
  [] -> Nothing
  moves -> Just $ do
    m <- moves
    pure (en + cost fr po m, updateKey fr m wo)

dropDeadEnds :: [Maybe [World]] -> [World]
dropDeadEnds [] = []
dropDeadEnds (Nothing : ws) = dropDeadEnds ws
dropDeadEnds (Just w : ws) = w ++ dropDeadEnds ws

sieve :: (a -> Bool) -> [a] -> ([a], [a])
sieve pre aa' = recur aa' ([], [])
  where
    recur [] acc = acc
    recur (a : aa) (p, notP) = recur aa $ if pre a then (a : p, notP) else (p, a : notP)

solve :: Int -> World -> [World]
solve cutOff w =
  let pods :: [(Point, Pod)]
      pods = M.assocs . snd $ w
      newWorlds = dropDeadEnds (move w <$> pods)
      (win, open) = sieve ((== winState) . snd) newWorlds
   in if null open then win else win ++ mconcat (solve cutOff <$> open)

solve2 :: Int -> World -> [World]
solve2 cutOff w =
  let pods :: [(Point, Pod)]
      pods = M.assocs . snd $ w
      newWorlds = dropDeadEnds (move2 w <$> pods)
      (win, open) = sieve ((== winState2) . snd) newWorlds
   in if null open then win else win ++ mconcat (solve2 cutOff <$> open)

demoWorld :: World
demoWorld = (0, demoState)

demoWorld2 :: World
demoWorld2 = (0, demoState2)

winState :: M.Map Point Pod
winState =
  M.fromList
    [ ((2, 1), Amber),
      ((2, 2), Amber),
      ((4, 1), Bronze),
      ((4, 2), Bronze),
      ((6, 1), Copper),
      ((6, 2), Copper),
      ((8, 1), Desert),
      ((8, 2), Desert)
    ]

winState2 :: M.Map Point Pod
winState2 =
  M.fromList
    [ ((2, 1), Amber),
      ((2, 2), Amber),
      ((2, 3), Amber),
      ((2, 4), Amber),
      ((4, 1), Bronze),
      ((4, 2), Bronze),
      ((4, 3), Bronze),
      ((4, 4), Bronze),
      ((6, 1), Copper),
      ((6, 2), Copper),
      ((6, 3), Copper),
      ((6, 4), Copper),
      ((8, 1), Desert),
      ((8, 2), Desert),
      ((8, 3), Desert),
      ((8, 4), Desert)
    ]

demoState :: M.Map Point Pod
demoState =
  M.fromList
    [ ((2, 1), Bronze),
      ((2, 2), Amber),
      ((4, 1), Copper),
      ((4, 2), Desert),
      ((6, 1), Bronze),
      ((6, 2), Copper),
      ((8, 1), Desert),
      ((8, 2), Amber)
    ]

demoState2 :: M.Map Point Pod
demoState2 =
  M.fromList
    [ ((2, 1), Bronze),
      ((2, 2), Desert),
      ((2, 3), Desert),
      ((2, 4), Amber),
      ((4, 1), Copper),
      ((4, 2), Copper),
      ((4, 3), Bronze),
      ((4, 4), Desert),
      ((6, 1), Bronze),
      ((6, 2), Bronze),
      ((6, 3), Amber),
      ((6, 4), Copper),
      ((8, 1), Desert),
      ((8, 2), Amber),
      ((8, 3), Copper),
      ((8, 4), Amber)
    ]

inputWorld :: World
inputWorld = (0, inputState)

inputWorld2 :: World
inputWorld2 = (0, inputState2)

inputState :: M.Map Point Pod
inputState =
  M.fromList
    [ ((2, 1), Desert),
      ((2, 2), Copper),
      ((4, 1), Desert),
      ((4, 2), Copper),
      ((6, 1), Amber),
      ((6, 2), Bronze),
      ((8, 1), Amber),
      ((8, 2), Bronze)
    ]

inputState2 :: M.Map Point Pod
inputState2 =
  M.fromList
    [ ((2, 1), Desert),
      ((2, 2), Desert),
      ((2, 3), Desert),
      ((2, 4), Copper),
      ((4, 1), Desert),
      ((4, 2), Copper),
      ((4, 3), Bronze),
      ((4, 4), Copper),
      ((6, 1), Amber),
      ((6, 2), Bronze),
      ((6, 3), Amber),
      ((6, 4), Bronze),
      ((8, 1), Amber),
      ((8, 2), Amber),
      ((8, 3), Copper),
      ((8, 4), Bronze)
    ]

cost :: Point -> Pod -> Point -> Int
cost (xf, yf) p (xt, yt) | yf /= 0 && yt /= 0 = cost (xf, yf) p (xf, 0) + cost (xf, 0) p (xt, yt)
cost fr p to = manhattanDistance fr to * energy p
  where
    manhattanDistance :: Point -> Point -> Int
    manhattanDistance p1 p2 =
      let dx = fst p1 - fst p2
          dy = snd p1 - snd p2
       in abs dx + abs dy

energy :: Pod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

inputParser :: Parser [String]
inputParser = undefined

-- 16489
solution1 :: Input -> Int
solution1 _input =
  solve 16491 inputWorld
    |> fmap fst
    |> minimum

-- 43413
solution2 :: Input -> Int
solution2 _input =
  solve2 99999 inputWorld2
    --  solve2 99999 demoWorld2
    |> fmap fst
    |> minimum

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 16489 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 43413 . solution2 =<< input
    ]

testData :: Input
testData = "no parsing, input is hardcoded"
