{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC11 where

import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), times)
import Text.Parsec (many1, digit, many, newline)
import Data.Char (digitToInt)
import Data.List (sortOn)
import qualified Data.Map.Strict as M

data Octopus = Energy Int | Flash deriving (Eq)

instance Show Octopus where
  show Flash = "f"
  show (Energy n) = show n

type Cave = M.Map Point Octopus
type Point = (Int, Int)


lineParser :: Parser [Octopus]
lineParser = do
  chars <- many1 digit
  pure $ Energy . digitToInt <$> chars

inputParser :: Parser Cave
inputParser = do
  listOfLists <- many1 (lineParser <* many newline)
  pure $ M.fromList (kvPairs listOfLists)
  where kvPairs :: [[Octopus]] -> [(Point, Octopus)]
        kvPairs os = do
          x <- [0 .. length (head os) - 1]
          y <- [0 .. (length os) - 1]
          pure ((x, y), (os !! y) !! x)


neighbours :: Point -> Cave ->[Point]
neighbours (x, y) c = filter bounds $ do
      dx <- [-1 .. 1]
      dy <- [-1 .. 1]
      pure (x+dx, y+dy)
  where
    ((mx, my),_) = M.findMax c
    bounds (x', _) | x' < 0 || x' > mx = False
    bounds (_, y') | y' < 0 || y' > my = False
    bounds (x',y') | x == x' && y == y' = False
    bounds _ = True

inc :: Octopus -> Octopus
inc Flash = Flash
-- inc (Energy 9) = Flash
inc (Energy n) = Energy (n + 1)

step0 :: Cave -> (Cave, Int)
step0 c = step (c, 0)

step :: (Cave, Int) -> (Cave, Int)
step (c, fs) = M.map (inc) c |> flash' |> fmap (+ fs)

updateAllWithKey :: (k -> a -> M.Map k a -> M.Map k a) -> M.Map k a -> M.Map k a
updateAllWithKey f c = M.foldrWithKey f c c

tagFlashed :: Cave -> (Cave, [Point])
tagFlashed c = M.foldrWithKey f (c,[]) c
  where f p (Energy e) (c,fs) | e > 9 = (M.adjust (const Flash) p c, p:fs)
        f _ _ acc = acc

flash' :: Cave -> (Cave, Int)
flash' c = let (c', fs) = tagFlashed c
           in if null fs then (updateAllWithKey resetFlashed c', M.foldr countFlashed 0 c')
                          else flash' $ foldr f c' fs
  where f :: Point -> Cave -> Cave
        f p acc = flashNeighbours p ((M.!) acc p) acc

flash :: Cave -> (Cave, Int)
flash c = let c' = updateAllWithKey flashNeighbours c in
  (updateAllWithKey resetFlashed c', M.foldr countFlashed 0 c')

flashNeighbours :: Point -> Octopus -> Cave -> Cave
flashNeighbours p Flash cave = updateNeighbours p cave
flashNeighbours _p _octopus cave = cave

resetFlashed :: Point -> Octopus -> Cave -> Cave
resetFlashed p Flash c = M.adjust (const (Energy 0)) p c
resetFlashed _p _o c = c

countFlashed :: Octopus -> Int -> Int
countFlashed Flash f = f + 1
countFlashed _o f = f

updateNeighbours :: Point -> Cave -> Cave
updateNeighbours p cave = let ns = neighbours p cave in foldr ( (M.adjust inc)) cave ns

linesAt :: Int -> String -> [String]
linesAt _n [] = []
linesAt n s = take n s : linesAt n (drop n s)

switch :: ((a,a),b) -> ((a,a),b)
switch ((a,a'),b) = ((a',a),b)

pretty :: Cave -> IO ()
pretty c = M.toList c |> fmap switch |> sortOn fst |> fmap snd |> fmap show |> mconcat |> linesAt (maxL + 1) |> fmap putStrLn |> mconcat
  where ((maxL,_),_) = M.findMax c

countTilAllZero :: Int -> Cave -> Int
countTilAllZer0 10000 c = error "run away"
countTilAllZero n c = if M.toList c |> fmap snd |> all ((==) (Energy 0)) then n else  countTilAllZero (n + 1) (fst $ (step0 c))

-- 1694
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> step0
    |> times 99 step
    |> snd

-- 346
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> countTilAllZero 0

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 1694 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 346 . solution2 =<< input
    ]

testData :: Input
testData = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526\n"

realData :: Input
realData = "8258741254\n3335286211\n8468661311\n6164578353\n2138414553\n1785385447\n3441133751\n3586862837\n7568272878\n6833643144\n"

small :: Input
small = "999\n919\n999\n"