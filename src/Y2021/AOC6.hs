{-# LANGUAGE TupleSections #-}

module Y2021.AOC6 where

import AOC (Solution (PureSolution))
import qualified Data.Map.Strict as M
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (char, sepBy)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, times, (|>))

type Fish = Int

evolve :: Fish -> [Fish]
evolve 0 = [6, 8]
evolve n = [n -1]

simulate :: [Fish] -> Int -> [Fish]
simulate fishes 0 = fishes
simulate fishes n = simulate (mconcat $ evolve <$> fishes) (n - 1)

inputParser :: Parser [Fish]
inputParser = sepBy number (char ',')

solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> flip simulate 80
    |> length

-- 1609314870967
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> toTank
    |> times 256 evolveTank
    |> countFish

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 353274 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 1609314870967 . solution2 =<< input
    ]

type FishTank = M.Map Int Int

emptyTank :: FishTank
emptyTank = M.fromList $ (,0) <$> [0 .. 8]

toTank :: [Fish] -> FishTank
toTank = foldl (\m n -> M.insertWith (+) n 1 m) emptyTank

evolveTank :: FishTank -> FishTank
evolveTank = M.foldrWithKey f emptyTank
  where
    f :: Int -> Int -> FishTank -> FishTank
    f 0 fish m = M.update (Just . (+ fish)) 6 $ M.update (Just . (+ fish)) 8 m
    f k fish m = M.update (Just . (+ fish)) (k - 1) m

countFish :: FishTank -> Int
countFish = M.foldr (+) 0

solution :: Solution
solution = PureSolution solution1 353274 solution2 1609314870967
