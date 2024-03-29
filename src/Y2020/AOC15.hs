module Y2020.AOC15 where

import AOC (Solution (PureSolution))
import Data.List (elemIndex, unfoldr)
import qualified Data.Map.Strict as M
import Util (Input)

start :: [Int]
start = [6, 19, 0, 5, 7, 13, 1]

--start = [0,3,6] -- 436
--start = [3,1,2] -- 1836

memorySequence :: [Int] -> [Int]
memorySequence strt = strt ++ unfoldr f (reverse strt)
  where
    f :: [Int] -> Maybe (Int, [Int])
    f l@(x : xs) | x `notElem` xs = Just (0, 0 : l)
    f l@(x : xs) = do
      idx <- x `elemIndex` xs
      return (idx + 1, idx + 1 : l)
    f _ = error "something failed"

memorySequence' :: [Int] -> [Int]
memorySequence' startingList = startingList ++ unfoldr f (last startingList, initialCache, length startingList - 1)
  where
    initialCache = M.fromList $ init $ zip startingList [0 ..]
    f :: (Int, M.Map Int Int, Int) -> Maybe (Int, (Int, M.Map Int Int, Int))
    f (lastNum, cache, count) | lastNum `M.notMember` cache = Just (0, (0, M.insert lastNum count cache, count + 1))
    f (lastNum, cache, count) = do
      lastSeenRound <- M.lookup lastNum cache
      let next = count - lastSeenRound
      return (next, (next, M.insert lastNum count cache, count + 1))

-- 468
solution1 :: Input -> Int
solution1 _input = memorySequence' start !! (2020 - 1)

-- 1801753
solution2 :: Input -> Int
solution2 _input = memorySequence' start !! (30000000 - 1)

solution :: Solution
solution = PureSolution solution1 468 solution2 1801753
