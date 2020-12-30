module AOC25 where

import           Data.List (foldl')

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
nTimes n f x = foldl' (\a _i -> f a) x [1..n]

bruteforceLoop :: PubKey -> SubjectNumber -> LoopSize
bruteforceLoop pk sn = fst . head $ filter (\(_l,key) -> key == pk) $ zip [0..] $ iterate (transform sn) 1

-- 9620012
solution1 :: IO Int
solution1 = do
  let keypair = input
  let loopFst = bruteforceLoop (fst keypair) 7
--  let loopSnd = bruteforceLoop (snd keypair) 7
  return $ nTimes loopFst (transform (snd keypair)) 1
--  return $ nTimes loopSnd (transform (fst keypair)) 1


solution2 :: IO Int
solution2 = return 0
