{-# LANGUAGE OverloadedStrings #-}

module Y2015.AOC24 where

import AOC (Solution (PureSolution))
import Data.Function (on)
import Data.List (sort, sortBy)
import Util (Input, (|>))

type PackageWeight = Int

threeWaySums :: (Ord a, Num a) => a -> [a] -> [(a, a, a)] -> [(a, a, a)]
threeWaySums _ [] t = t
threeWaySums target (a : as) [] = threeWaySums target as [(a, 0, 0), (0, a, 0), (0, 0, a)]
threeWaySums target (a : as) acc = threeWaySums target as $ mconcat $ fmap (filter overshot) $ (\(x, y, z) -> [(a + x, y, z)] <> [(x, a + y, z)] <> [(x, y, a + z)]) <$> acc
  where
    overshot (x, y, z) | x > target || y > target || z > target = False
    overshot _ = True

twoWaySplit :: [a] -> [([a], [a])] -> [([a], [a])]
twoWaySplit [] t = t
twoWaySplit (a : as) [] = twoWaySplit as [([a], []), ([], [a]), ([], [])]
twoWaySplit (a : as) acc = twoWaySplit as $ mconcat $ (\(x, y) -> [(a : x, y)] <> [(x, a : y)]) <$> acc

firstPass :: (Eq a, Num a) => Int -> a -> ([a], [a]) -> Bool
firstPass _ _ ([], _) = False
firstPass _ _ (_, []) = False
firstPass cutoff _ (a, _) | length a > cutoff = False -- guess based optimisation
firstPass _ n (a, b) | n * sum a == sum b = True
firstPass _ _ _ = False

secondPass :: (Eq a, Num a) => ([a], [a]) -> Bool
secondPass (_, bs) = twoWaySplit bs [] |> any (\(a, b) -> sum a == sum b)

secondPass' :: (Ord a, Num a) => ([a], [a]) -> Bool
secondPass' (as, bs) = threeWaySums (sum as) bs [] |> any (\(a, b, c) -> a == b && b == c)

sortOnLegRoom :: [[PackageWeight]] -> [[PackageWeight]]
sortOnLegRoom = sortBy (compare `on` length)

takeCandidates :: [[PackageWeight]] -> [[PackageWeight]]
takeCandidates [] = []
takeCandidates as@(a : _) = takeWhile (\c -> length c == length a) as

quantumEntanglement :: [PackageWeight] -> Int
quantumEntanglement = product

distributions, distributions' :: [PackageWeight] -> [[PackageWeight]]
distributions packets = twoWaySplit packets [] |> filter (firstPass 7 2) |> filter secondPass |> fmap fst
distributions' packets = twoWaySplit packets [] |> filter (firstPass 5 3) |> filter secondPass' |> fmap fst

-- 11266889531
solution1 :: Input -> Int
solution1 _input =
  distributions input
    |> sortOnLegRoom
    |> takeCandidates
    |> fmap quantumEntanglement
    |> sort
    |> head

-- 77387711
solution2 :: Input -> Int
solution2 _input =
  distributions' input
    |> sortOnLegRoom
    |> takeCandidates
    |> fmap quantumEntanglement
    |> sort
    |> head

example :: [PackageWeight]
example = [1 .. 5] <> [7 .. 11]

input :: [PackageWeight]
input =
  [ 1,
    3,
    5,
    11,
    13,
    17,
    19,
    23,
    29,
    31,
    41,
    43,
    47,
    53,
    59,
    61,
    67,
    71,
    73,
    79,
    83,
    89,
    97,
    101,
    103,
    107,
    109,
    113
  ]

solution :: Solution
solution = PureSolution solution1 11266889531 solution2 77387711
