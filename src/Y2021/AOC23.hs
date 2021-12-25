{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC23 where

import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import qualified Data.Map.Strict as M

--  01234567891
-- #############
-- #...........# 0
-- ###B#C#B#D### 1
--   #A#D#C#A#   2
--   #########

type Point = (Int, Int)

validPositions :: [Point]
validPositions = (2,1):(2,2):(4,1):(4,2):(6,1):(6,2):(8,1):(8,2): do
  x <- [0..1]
  pure (x,0)

type World = (Int, M.Map Point Pod)

demoWorld :: World
demoWorld = (0, demoState)

winState :: M.Map Point Pod
winState = M.fromList [
  ((2,1),Amber),
  ((2,2),Amber),
  ((4,1),Bronze),
  ((4,2),Bronze),
  ((6,1),Copper),
  ((6,2),Copper),
  ((8,1),Desert),
  ((8,2),Desert)
  ]

demoState :: M.Map Point Pod
demoState = M.fromList [
  ((2,1),Bronze),
  ((2,2),Amber),
  ((4,1),Copper),
  ((4,2),Desert),
  ((6,1),Bronze),
  ((6,2),Copper),
  ((8,1),Desert),
  ((8,2),Amber)
  ]

data Pod = Amber | Bronze | Copper | Desert
  deriving (Show, Eq, Ord)

data Burrow = Burow {
  roomA :: [Pod],
  roomB :: [Pod],
  roomC :: [Pod],
  roomD :: [Pod],
  leftHall :: [Pod],
  hall1 :: [Pod],
  hall2 :: [Pod],
  hall3 :: [Pod],
  rightHall :: [Pod]
}

energy :: Pod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

inputParser :: Parser [String]
inputParser = undefined

solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> error "not yet"

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> error "not yet"

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" undefined . solution1 =<< input,
      TestCase $ assertEqual "solution 2" undefined . solution2 =<< input
    ]

testData :: Input
testData = ""