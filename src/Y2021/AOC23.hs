{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC23 where

import AOC (Solution (PureSolution))
import Data.List (delete)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
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

type RoomSpace = Int

validPositions :: RoomSpace -> [Point]
validPositions rs =
  do
    room <- [2, 4, 6, 8]
    space <- [1 .. rs]
    pure (room, space)
    ++ do
      x <- [0 .. 10]
      pure (x, 0)

validTargets :: RoomSpace -> [Point]
validTargets rs = foldl (flip delete) (validPositions rs) [(2, 0), (4, 0), (6, 0), (8, 0)]

isPossible :: RoomSpace -> M.Map Point Pod -> Point -> Pod -> Point -> Bool
isPossible _rs _m fr _ to | fr == to = False -- can't move in place
isPossible _rs _m (_xf, 0) _p (_xt, 0) = False -- can't go from hallway to hallway
isPossible _rs _m _fr Amber (xt, yt) | yt /= 0 && xt /= 2 = False -- amber can only go into first room
isPossible _rs _m _fr Bronze (xt, yt) | yt /= 0 && xt /= 4 = False -- bronze can only go into second room
isPossible _rs _m _fr Copper (xt, yt) | yt /= 0 && xt /= 6 = False -- copper can only go into third room
isPossible _rs _m _fr Desert (xt, yt) | yt /= 0 && xt /= 8 = False -- desert can only go into fourth room
isPossible rs m _fr _p (xt, yt) | yt > 0 && any (\y -> M.notMember (xt, y) m) [(yt + 1) .. rs] = False -- has to go into room all the way
isPossible rs m _fr p (xt, yt) | yt > 0 && any (\y -> M.lookup (xt, y) m /= Just p) [(yt + 1) .. rs] = False -- can't go into room that has wrong pod
isPossible rs _m (2, yt) Amber _ | yt == rs = False -- happy amber never moves
isPossible rs _m (4, yt) Bronze _ | yt == rs = False -- happy bronze never moves
isPossible rs _m (6, yt) Copper _ | yt == rs = False -- happy copper never moves
isPossible rs _m (8, yt) Desert _ | yt == rs = False -- happy desert never moves
isPossible rs m (2, yt) Amber _ | all (\y -> M.lookup (2, y) m == Just Amber) [yt .. rs] = False -- happy amber never moves
isPossible rs m (4, yt) Bronze _ | all (\y -> M.lookup (4, y) m == Just Bronze) [yt .. rs] = False -- happy amber never moves
isPossible rs m (6, yt) Copper _ | all (\y -> M.lookup (6, y) m == Just Copper) [yt .. rs] = False -- happy amber never moves
isPossible rs m (8, yt) Desert _ | all (\y -> M.lookup (8, y) m == Just Desert) [yt .. rs] = False -- happy amber never moves
isPossible _rs m _ _ t | M.member t m = False -- can only go to free fields
isPossible _rs m (xf, yf) _ _to | yf /= 0 && any (\y -> M.member (xf, y) m) [1 .. (yf - 1)] = False -- Can only leave room if no one is in the way
isPossible _rs _m (xf, _yf) _ (xt, _yt) | xf == xt = False -- has to leave the room
isPossible _rs m fr@(xf, _) _ (xt, _) | any (\x -> M.member (x, 0) (M.delete fr m)) [(min xf xt) .. (max xf xt)] = False -- path has to be free
isPossible rs m _ Amber (_, 0) | all (\y -> isNothing (M.lookup (2, y) m) || M.lookup (2, y) m == Just Amber) [1 .. rs] = False -- Amber has to go home directly if possible
isPossible rs m _ Bronze (_, 0) | all (\y -> isNothing (M.lookup (4, y) m) || M.lookup (4, y) m == Just Bronze) [1 .. rs] = False -- Bronze has to go home directly if possible
isPossible rs m _ Copper (_, 0) | all (\y -> isNothing (M.lookup (6, y) m) || M.lookup (6, y) m == Just Copper) [1 .. rs] = False -- Copper has to go home directly if possible
isPossible rs m _ Desert (_, 0) | all (\y -> isNothing (M.lookup (8, y) m) || M.lookup (8, y) m == Just Desert) [1 .. rs] = False -- Desert has to go home directly if possible
isPossible _ _ _ _ _ = True

updateKey :: Ord k => k -> k -> M.Map k a -> M.Map k a
updateKey k k1 m = case M.lookup k m of
  Nothing -> m
  Just a -> M.delete k m |> M.insert k1 a

move :: RoomSpace -> World -> (Point, Pod) -> Maybe [World]
move rs (en, wo) (fr, po) = case filter (isPossible rs wo fr po) (validTargets rs) of
  [] -> Nothing
  --  moves -> trace (show fr ++ show po ++ " " ++ show moves ++ "\n" ++ pretty (en, wo)) $ Just $ do
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

solve :: RoomSpace -> Int -> World -> [World]
solve rs cutOff w =
  let pods :: [(Point, Pod)]
      pods = M.assocs . snd $ w
      newWorlds = dropDeadEnds (move rs w <$> pods)
      (win, open) = sieve ((== winState rs) . snd) newWorlds
   in if null open then win else win ++ mconcat (solve rs cutOff <$> open)

demoWorld :: World
demoWorld = (0, demoState)

demoWorld2 :: World
demoWorld2 = (0, demoState2)

winState :: RoomSpace -> M.Map Point Pod
winState rs =
  M.fromList $
    [1 .. rs]
      >>= ( \r ->
              [ ((2, r), Amber),
                ((4, r), Bronze),
                ((6, r), Copper),
                ((8, r), Desert)
              ]
          )

inputWorld :: World
inputWorld = (0, inputState)

inputWorld2 :: World
inputWorld2 = (0, inputState2)

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
  solve 2 16491 inputWorld
    |> fmap fst
    |> minimum

-- 43413
solution2 :: Input -> Int
solution2 _input =
  solve 4 50000 inputWorld2
    |> fmap fst
    |> minimum

testData :: Input
testData = "no parsing, input is hardcoded"

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

solution :: Solution
solution = PureSolution solution1 16489 solution2 43413
