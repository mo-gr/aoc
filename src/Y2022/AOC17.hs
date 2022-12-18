{-# LANGUAGE OverloadedStrings #-}

module Y2022.AOC17 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Set as S
import Text.Parsec (char, many)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import Data.Bifunctor (first, second)
import Debug.Trace
import Data.List (scanl', findIndex, elemIndex)

data Jet = L | R

data Tile = HLine | Plus | Angle | VLine | Square

type Point = (Int, Int)
type Field = S.Set Point
type Shape = S.Set Point

inputParser :: Parser [Jet]
inputParser = many ((char '>' $> R) <|> (char '<' $> L))

pointsAt :: Point -> Tile -> S.Set Point
pointsAt (x, y) HLine = S.fromList [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)]
pointsAt (x, y) Angle = S.fromList [(x, y), (x + 1, y), (x + 2, y), (x + 2, y + 1), (x + 2, y + 2)]
pointsAt (x, y) VLine = S.fromList [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)]
pointsAt (x, y) Square = S.fromList [(x, y), (x + 1, y), (x + 1, y + 1), (x, y + 1)]
pointsAt (x, y) Plus = S.fromList [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x + 1, y + 2), (x + 2, y + 1)]

maxHeight :: Field -> Int
maxHeight f | S.null f = 0
maxHeight f = S.elems f |> fmap snd |> maximum

spawnPoint :: Field -> Point
spawnPoint f = (2, 4 + maxHeight f)

jet :: Field -> Shape -> Jet -> Shape
jet _ s L | S.findMin s |> fst <= 0 = s
jet f s L = if S.disjoint f s' then s' else s
  where s' = S.map (first pred) s
jet _ s R | S.findMax s |> fst >= 6 = s
jet f s R = if S.disjoint f s' then s' else s
  where s' = S.map (first succ) s


moveDown :: Field -> Shape -> Either Field Shape
moveDown f s = let s' = S.map (second pred) s in
  if S.disjoint f s' then Right s' else Left $ S.union f s

dropOrder :: [Tile]
dropOrder = cycle [HLine, Plus, Angle, VLine, Square]

newField :: Field
newField = S.fromList $ do
  x <- [0..6]
  [(x,0)]

step :: Field -> Jet -> Shape -> Either Field Shape
step f j s = jet f s j |> moveDown f

dropTile :: Field -> Shape -> [Jet] -> ([Jet], Field)
dropTile f s js = case step f (head js) s of
  Right s' -> dropTile f s' (tail js)
  Left f' -> (tail js, f')


solve1 :: [Jet] -> Int
solve1 jsInput = foldl go (cycle jsInput,newField) (take 2022 dropOrder)
            |> snd |> maxHeight
  where go :: ([Jet], Field) -> Tile -> ([Jet], Field)
        go (js, f) t = dropTile f (pointsAt (spawnPoint f) t) js


-- 3168
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> solve1

-- cycle detection from rosetta code https://rosettacode.org/wiki/Cycle_detection#Haskell
findCycle :: Eq a => [a] -> Maybe ([a], Int, Int)
findCycle lst =
  do l <- findCycleLength lst
     mu <- findIndex (uncurry (==)) $ zip lst (drop l lst)
     let c = take l $ drop mu lst
     return (c, l, mu)

findCycleLength :: Eq a => [a] -> Maybe Int
findCycleLength [] = Nothing
findCycleLength (a:as) =
  let loop _ _ _ [] = Nothing
      loop pow lam x (y:ys)
        | x == y     = Just lam
        | pow == lam = loop (2*pow) 1 y ys
        | otherwise  = loop pow (1+lam) x ys
  in loop 1 1 a as

--solve2 :: [Jet] -> Int
solve2 jsInput = scanl' go (cycle jsInput,newField) (take (length jsInput * 5) dropOrder)
            |> fmap snd |> fmap maxHeight |> (\mh -> zipWith (flip (-)) mh (tail mh)) |> findCycle
  where go :: ([Jet], Field) -> Tile -> ([Jet], Field)
        go (js, f) t = dropTile f (pointsAt (spawnPoint f) t) js

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> solve1

solution :: Solution
solution = PureSolution solution1 3168 solution2 undefined

testData :: Input
testData = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\n"