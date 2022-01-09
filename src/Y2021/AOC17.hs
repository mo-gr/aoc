module Y2021.AOC17 where

import AOC (Solution (PureSolution))
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (string)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, parseOrDie, (|>))

type Range = ([Int], [Int])

type Point = (Int, Int)

type Velocity = (Int, Int)

type Probe = (Point, Velocity)

data Trajectory = Hit | Overshot | InFlight

inputParser :: Parser Range
inputParser = do
  _ <- string "target area: x="
  rx <- negativeNumber <* string ".."
  rx' <- negativeNumber
  _ <- string ", y="
  ry <- negativeNumber <* string ".."
  ry' <- negativeNumber
  pure ([rx .. rx'], [ry .. ry'])

mkProbe :: Velocity -> Probe
mkProbe v = ((0, 0), v)

probeY :: Probe -> Int
probeY ((_, y), _) = y

step :: Probe -> Probe
step ((px, py), (vx, vy)) = ((px + vx, py + vy), (drag vx, vy - 1))
  where
    drag :: Int -> Int
    drag 0 = 0
    drag x | x > 0 = x - 1
    drag x = x + 1

trajectory :: Range -> Probe -> Trajectory
trajectory (xr, yr) ((px, py), _v) | px `elem` xr && py `elem` yr = Hit
trajectory (_xr, yr) ((_px, py), (_vx, vy)) | vy < 0 && py < minimum yr = Overshot
trajectory (xr, _yr) ((px, _py), (vx, _vy)) | vx == 0 && px < minimum xr = Overshot
trajectory (xr, _yr) ((px, _py), (vx, _vy)) | vx == 0 && px > maximum xr = Overshot
trajectory _ _ = InFlight

launch :: Range -> Point -> Probe -> Maybe Point
launch r p_max@(_, y_max) probe = case trajectory r probe of
  Hit -> if probeY probe > y_max then Just (fst probe) else Just p_max
  Overshot -> Nothing
  InFlight -> if probeY probe > y_max then launch r (fst probe) (step probe) else launch r p_max (step probe)

cmpSnd :: Point -> Point -> Ordering
cmpSnd (_, y) (_, y') = compare y y'

shotgun :: Range -> Point
shotgun r@(rx, _ry) = maximumBy cmpSnd $
  catMaybes $ do
    x <- [0 .. (maximum rx)]
    y <- [0 .. (maximum rx)]
    pure $ launch r (0, 0) $ mkProbe (x, y)

countShotgunHits :: Range -> Int
countShotgunHits r@(rx, ry) = length $
  catMaybes $ do
    x <- [0 .. (maximum rx)]
    y <- [(minimum ry) .. (maximum rx)]
    pure $ launch r (0, 0) $ mkProbe (x, y)

-- 5671
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> shotgun
    |> snd

-- 4556
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> countShotgunHits

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 5671 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 4556 . solution2 =<< input
    ]

solution :: Solution
solution = PureSolution solution1 solution2 verify
