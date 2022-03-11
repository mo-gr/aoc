{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Y2016.AOC19 (solution) where

import AOC (Solution (PureSolution))
import Data.Sequence (Seq, deleteAt, fromList, (|>), pattern Empty, pattern (:<|))
import Util (Input)

type Rule b = Seq b -> Either (Seq b) b

eliminateNext :: Rule b
eliminateNext (w :<| Empty) = Right w
eliminateNext (a :<| _ :<| rst) = Left $ rst |> a
eliminateNext _ = error "this should not happen"

eliminateOpposite :: Rule b
eliminateOpposite (w :<| Empty) = Right w
eliminateOpposite es@(a :<| rst) = let half = length es `quot` 2 in Left $ deleteAt (half - 1) rst |> a
eliminateOpposite _ = error "this should not happen"

play :: Rule b -> Seq b -> b
play rules es = case rules es of
  Right winner -> winner
  Left !es' -> play rules es'

-- 1834471
solution1 :: Input -> Int
solution1 _input = play eliminateNext $ fromList [1 .. 3014387]

-- 1420064
solution2 :: Input -> Int
solution2 _input = play eliminateOpposite $ fromList [1 .. 3014387]

solution :: Solution
solution = PureSolution solution1 1834471 solution2 1420064
