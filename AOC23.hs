{-# LANGUAGE BangPatterns #-}

module AOC23 where

import Data.List (intercalate, foldl')
import Data.Int (Int32)
import Debug.Trace
import qualified Data.Sequence as S
import Data.Foldable (toList)


-- 46978532
solution1 :: IO String
solution1 = return . result $ nTimes 100 move (minimum input, maximum input, S.fromList input)

solution2 :: IO Cup
solution2 = return . result2 $! nTimes 10000000 move $! part2 input

part2 :: [Cup] -> Game
part2 game = let g = take 1000000 $ game ++ [(maximum game)..] in (minimum g, maximum g, S.fromList g)

type Cup = Int32

type Game = (Int32,Int32,S.Seq Cup)

destinationCup :: Game -> Cup -> (Cup, Cup, Cup) -> Cup
destinationCup g d p@(p1, _p2, _p3) | d - 1 == p1 = destinationCup g (d - 1) p
destinationCup g d p@(_p1, p2, _p3) | d - 1 == p2 = destinationCup g (d - 1) p
destinationCup g d p@(_p1, _p2, p3) | d - 1 == p3 = destinationCup g (d - 1) p
destinationCup g@(mini,maxi,_game) d p = if (d - 1) < mini then destinationCup g (maxi + 1) p else d - 1

move :: Game -> Game
move g@(mini, maxi, !ss) =
  let c = S.index ss 0
      x = S.index ss 1
      y = S.index ss 2
      z = S.index ss 3
      xs = S.drop 4 ss
      !destination = destinationCup g c (x,y,z)
--      !after = S.takeWhileL (/= destination) xs
--                 S.>< S.singleton destination
--                 S.>< S.fromList [x,y,z]
--                 S.>< S.drop 1 ( S.dropWhileL (/=  destination) xs)
--                 S.>< S.singleton c
      Just dix = S.elemIndexL destination xs
      (b,a) = S.splitAt dix xs
      !after = b S.>< S.fromList [destination,x,y,z] S.>< S.drop 1 a S.>< S.singleton c 
  in (mini, maxi, after)

result :: Game -> String
result (_,_,g) = intercalate "" $ fmap show $ take (length g - 1) $ drop 1 $ dropWhile (/= 1) $ toList g ++ toList g

result2 :: Game -> Cup
result2 (_,_,g) = product $ take 2 $ drop 1 $ dropWhile (/= 1) $ cycle $ toList g

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = foldl' (\a _i -> f a) x [1..n]

example :: [Cup]
example = [3,8,9,1,2,5,4,6,7]

input :: [Cup]
input = [2, 1, 5, 6, 9, 4, 7, 8, 3]
