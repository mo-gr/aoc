module Y2020.AOC25 where

import AOC (Solution (PureSolution))
import Data.List (foldl')
import Util (Input)

type PubKey = Int

type LoopSize = Int

type SubjectNumber = Int

type Value = Int

divisor :: Value
divisor = 20201227

transform :: SubjectNumber -> Value -> Value
transform sn value = (sn * value) `mod` divisor

example :: (PubKey, PubKey)
example = (5764801, 17807724)

input :: (PubKey, PubKey)
input = (3418282, 8719412)

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = foldl' (\a _i -> f a) x [1 .. n]

bruteforceLoop :: PubKey -> SubjectNumber -> LoopSize
bruteforceLoop pk sn = fst . head $ filter (\(_l, key) -> key == pk) $ zip [0 ..] $ iterate (transform sn) 1

-- 9620012
solution1 :: Input -> Int
solution1 _input =
  let keypair = input
      loopFst = bruteforceLoop (fst keypair) 7
   in nTimes loopFst (transform (snd keypair)) 1

solution2 :: Input -> Int
solution2 _input = 0

solution :: Solution
solution = PureSolution solution1 9620012 solution2 0
