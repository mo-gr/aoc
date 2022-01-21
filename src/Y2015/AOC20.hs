module Y2015.AOC20 where

import AOC (Solution (PureSolution))
import Data.List (findIndex, group)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Util (Input)

type HouseNumber = Int

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

aliquotSieve :: Int -> Int -> Int -> Maybe Int -> Int
aliquotSieve limit target numPresents quota =
  let houses = M.fromList [(address, address * numPresents) | address <- [0 .. limit]]
      bound Nothing _ = limit
      bound (Just q) elf = min limit (elf * q)
      candidates = foldl f (houses, S.empty) [2 .. limit]
      f acc elf =
        let b = bound quota elf
            ff (h, c) addr =
              let h' = M.adjust ((numPresents * elf) +) addr h
               in case M.lookup addr h' of
                    Just pcount | pcount >= target -> (h', S.insert addr c)
                    _ -> (h', c)
         in foldl ff acc [elf * 2, elf * 3 .. b + 1]
   in S.findMin $ snd candidates

-- python solution stolen from https://www.reddit.com/r/adventofcode/comments/po1zel/2015_day_20_there_must_be_a_more_efficient_way_to/
-- def aliquot_sieve(limit, target_presents, num_presents=10, quota=None):
--    houses = [address * num_presents for address in range(limit + 1)]
--    candidates = set([])
--    for elf in range(2, len(houses)):
--        bound = min(limit, elf * quota) if quota else limit
--        for address in range(elf * 2, bound + 1, elf):
--            houses[address] += num_presents * elf
--            if houses[address] >= target_presents:
--                candidates.add(address)
--    return min(candidates)

-- 776160
solution1 :: Input -> Int
solution1 _input = case findIndex (> input) $ presentsPrime <$> [0 ..] of
  Nothing -> error "something went wrong"
  Just x -> x

-- 786240
solution2 :: Input -> Int
solution2 _input = aliquotSieve sieveLimit input 11 (Just 50)

sieveLimit :: Int
sieveLimit = 1000000

input :: HouseNumber
input = 33100000

solution :: Solution
solution = PureSolution solution1 776160 solution2 786240
