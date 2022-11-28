{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC16 where

import AOC (Solution (PureSolution))
import Util (Input, (|>))

data Bit = I | O
  deriving (Show, Eq)

flipBits :: Bit -> Bit
flipBits I = O
flipBits O = I

dragonExpand :: [Bit] -> [Bit]
dragonExpand a = reverse a |> fmap flipBits |> \b -> a <> [O] <> b

checksum :: [Bit] -> [Bit]
checksum a =
  let c = checksumStep a
   in if even $ length c then checksum c else c

checksumStep :: [Bit] -> [Bit]
checksumStep (a : b : rst)
  | a == b = I : checksumStep rst
  | otherwise = O : checksumStep rst
checksumStep _ = []

fillData :: Int -> [Bit] -> [Bit]
fillData size bits
  | length bits >= size = checksum $ take size bits
  | otherwise = fillData size $ dragonExpand bits

showBits :: [Bit] -> String
showBits (I : rst) = '1' : showBits rst
showBits (O : rst) = '0' : showBits rst
showBits [] = []

-- 11101010111100010
solution1 :: Input -> String
solution1 _input =
  fillData 272 seed
    |> showBits

-- 01001101001000101
solution2 :: Input -> String
solution2 _input =
  fillData 35651584 seed
    |> showBits

seed, testSeed :: [Bit]
testSeed = [I, O, O, O, O]
seed = [I, O, I, I, I, O, I, I, I, I, I, O, O, I, I, I, I]

solution :: Solution
solution = PureSolution solution1 "11101010111100010" solution2 "01001101001000101"
