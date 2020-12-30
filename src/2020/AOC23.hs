{-# LANGUAGE NamedFieldPuns #-}

module AOC23 where

import           Data.List       (foldl', intercalate)
import qualified Data.Map.Strict as M


-- 46978532
solution1 :: IO String
solution1 = return . result $ nTimes 100 move $ setup input

-- 163035127721
solution2 :: IO Int
solution2 = return . result2 $ nTimes 10000000 move $ part2 input

part2 :: [Cup] -> Game
part2 game = let g = take 1000000 $ game ++ [(maximum game + 1)..] in setup g

setup ::  [Cup] -> Game
setup cups = Game {minCup=minimum cups, maxCup=maximum cups, successors=M.fromList $ zip cups (tail (cycle cups)), current=head cups}

type Cup = Int
data Game = Game {
  minCup::Cup,
  maxCup::Cup,
  successors::M.Map Cup Cup, 
  current::Cup
} deriving Show
  
destinationCup :: Game -> Cup -> (Cup, Cup, Cup) -> Cup
destinationCup g d p@(p1, _p2, _p3) | d - 1 == p1 = destinationCup g (d - 1) p
destinationCup g d p@(_p1, p2, _p3) | d - 1 == p2 = destinationCup g (d - 1) p
destinationCup g d p@(_p1, _p2, p3) | d - 1 == p3 = destinationCup g (d - 1) p
destinationCup g@Game{minCup, maxCup} d p = if (d - 1) < minCup then destinationCup g (maxCup + 1) p else d - 1

move :: Game -> Game
move g =
  let next3@(x,y,z) = takeNext3Cups g (current g)
      dest = destinationCup g (current g) next3
      cups' = M.insert (current g) (successors g M.! z)
              $ M.insert z (successors g M.! dest)
              $ M.insert y z
              $ M.insert x y
              $ M.insert dest x (successors g)
      curr' = case M.lookup (current g) cups' of
         Just c -> c
         _      -> error "something went wrong"
  in g {current=curr', successors=cups'}

result :: Game -> String
result g = intercalate "" $ fmap show $ drop 1 $  listify g

result2 :: Game -> Int
result2 Game{successors} = (successors M.! 1) * (successors M.! (successors M.! 1))

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = foldl' (\a _i -> f a) x [1..n]


listify :: Game -> [Cup]
listify Game {minCup, successors}= follow 9 minCup successors

takeNext3Cups :: Game -> Cup -> (Cup,Cup,Cup)
takeNext3Cups Game {successors} start = case follow 4 start successors of
   (_c:x:y:z:_) ->(x,y,z)
   _ -> error "something went wrongs"

follow :: Int -> Cup -> M.Map Cup Cup -> [Cup]
follow 0 _c _cups = []
follow n c cups = c : follow (n-1) (cups M.! c) cups

example :: [Cup]
example = [3,8,9,1,2,5,4,6,7]

input :: [Cup]
input = [2, 1, 5, 6, 9, 4, 7, 8, 3]
