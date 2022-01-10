module Y2020.AOC13 where

import AOC (Solution (PureSolution))
import Data.List (iterate', minimumBy)
import Util (Input)

type Minute = Int

makeBus :: Minute -> [Minute]
makeBus start = iterate' (+ start) start

example :: [Minute]
example = [7, 13, 59, 31, 19]

firstAvailable :: Minute -> [Minute] -> Minute
firstAvailable m ms = head $ dropWhile (<= m) ms

compareBusses :: (Int, Minute) -> (Int, Minute) -> Ordering
compareBusses (_busLine, bus) (_busLine', bus') = compare bus bus'

findFirst :: Minute -> [Minute] -> (Int, Minute)
findFirst departAfter busLines =
  let busses = makeBus <$> busLines
   in minimumBy compareBusses $ zip busLines (firstAvailable departAfter <$> busses)

result :: Minute -> (Int, Minute) -> Int
result departAfter (x, y) = x * (y - departAfter)

type Offset = Int

mkInput :: [(Offset, Minute)]
mkInput = filter (\(_offset, busId) -> busId > 0) $ zip [0 ..] [19, x, x, x, x, x, x, x, x, 41, x, x, x, x, x, x, x, x, x, 523, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 17, 13, x, x, x, x, x, x, x, x, x, x, 29, x, 853, x, x, x, x, x, 37, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 23]
  where
    x = -1

-- 1068781
mkExample :: [(Offset, Minute)]
mkExample = filter (\(_offset, busId) -> busId > 0) $ zip [0 ..] [7, 13, x, x, 59, x, 31, 19]
  where
    x = -1

-- 259
solution1 :: Input -> Int
solution1 _input = do
  let departAfter = 1000510
      busses = [19, 41, 523, 17, 13, 29, 853, 37, 23]
  result departAfter $ findFirst departAfter busses

-- 210612924879242
solution2 :: Input -> Int
solution2 _input = do
  let busses = mkInput
      start = 0
  findTime start 1 busses

solution :: Solution
solution = PureSolution solution1 259 solution2 210612924879242

findTime :: Minute -> Minute -> [(Offset, Minute)] -> Minute
findTime time _step [] = time
findTime time step bs@((offset, minute) : moreBusses) =
  if (time + step + offset) `mod` minute == 0
    then findTime (time + step) (lcm step minute) moreBusses
    else findTime (time + step) step bs
