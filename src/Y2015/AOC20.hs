module Y2015.AOC20 where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import Data.List (findIndex, group)
import Data.Word (Word64)
import Util (Input)

type HouseNumber = Word64

factorize :: HouseNumber -> HouseNumber -> [HouseNumber]
factorize _ 1 = []
factorize d n
  | d * d > n = [n]
  | n `mod` d == 0 = d : factorize d (n `div` d)
  | otherwise = factorize (d + 1) n

primeFactors :: HouseNumber -> [HouseNumber]
primeFactors = factorize 2

presentsPrime :: HouseNumber -> HouseNumber
presentsPrime n =
  let pfs = group $ primeFactors n
   in 10 * product ((\p -> sum ((^) (head p) <$> [0 .. (length p)])) <$> pfs)

presentsLazy :: HouseNumber -> HouseNumber
presentsLazy house = sum $ do
  elve <- [1 .. house]
  guard $ (elve * 50) > house
  guard $ (house `mod` elve) == 0
  pure (elve * 11)

-- 776160
solution1 :: Input -> Int
solution1 _input = case findIndex (> input) $ presentsPrime <$> [0 ..] of
  Nothing -> error "something went wrong"
  Just x -> x

-- too high 3009091
solution2 :: Input -> Int
solution2 _input = case findIndex (> input) $ presentsLazy <$> [0 ..] of
  Nothing -> error "something went wrong"
  Just x -> x

input :: HouseNumber
input = 33100000

solution :: Solution
solution = PureSolution solution1 776160 solution2 undefined
