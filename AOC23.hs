{-# LANGUAGE BangPatterns #-}

module AOC23 where

import Data.List (intercalate, foldl')
import Data.Int (Int32)
import Debug.Trace


-- 46978532
solution1 :: IO String
solution1 = return . result $ nTimes 100 move (minimum input, maximum input, length input, input)

solution2 :: IO String
solution2 = return . result2 $! nTimes 10000000 move $! part2 example

part2 :: [Cup] -> Game
part2 game = let g = take 1000000 $ game ++ [(maximum game)..] in (minimum g, maximum g, length g, g)

type Cup = Int32

type Game = (Int32,Int32,Int,[Cup])

destinationCup :: Game -> Cup -> (Cup, Cup, Cup) -> Cup
destinationCup g d p@(p1, _p2, _p3) | d - 1 == p1 = destinationCup g (d - 1) p
destinationCup g d p@(_p1, p2, _p3) | d - 1 == p2 = destinationCup g (d - 1) p
destinationCup g d p@(_p1, _p2, p3) | d - 1 == p3 = destinationCup g (d - 1) p
destinationCup g@(mini,maxi,_len,_game) d p = if (d - 1) < mini then destinationCup g (maxi + 1) p else d - 1

move :: Game -> Game
move g@(mini, maxi, len, c : x : y : z : xs) =
  let !destination = destinationCup g c (x,y,z) 
      !after = takeWhile (/= destination) (c:xs) ++ [destination] ++ [x,y,z] ++ drop 1 ( dropWhile (/=  destination) (c:xs))
  in (mini, maxi, len, id $! take len $ drop 1 $ dropWhile (/= c) $ cycle after)
move _ = error "invalid state"

result :: Game -> String
result (_,_,_,g) = intercalate "" $ fmap show $ take (length g - 1) $ drop 1 $ dropWhile (/= 1) $ cycle g

result2 :: Game -> String
result2 (_,_,_,g) = intercalate "" $ fmap show $ take 2 $ drop 1 $ dropWhile (/= 1) $ cycle g

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = foldl' (\a i -> traceShow i f a) x [1..n]

example :: [Cup]
example = [3,8,9,1,2,5,4,6,7]

input :: [Cup]
input = [2, 1, 5, 6, 9, 4, 7, 8, 3]
