module Y2020.AOC22 where

import qualified Data.Set as S

-- 32677
solution1 :: IO Int
solution1 = do
  return $ case play (player1, player2) of
    Right d -> count d
    Left d  -> count d

-- 33661
solution2 :: IO Int
solution2 = return $ case playRec S.empty (player1, player2) of
                Right d -> count d
                Left d  -> count d

play :: Game -> Either Deck Deck
play ([], p2)       = Right p2
play (p1, [])       = Left p1
play (c1:d1, c2:d2) | c1 > c2 = play (d1 ++ [c1,c2], d2)
play (c1:d1, c2:d2) = play (d1, d2 ++ [c2,c1])

playRec :: S.Set Game -> Game -> Either Deck Deck
playRec cache game | S.member game cache = Left $ fst game
playRec _cache ([], p2) = Right p2
playRec _cache (p1, []) = Left p1
playRec cache g@(c1:d1, c2:d2) | (length d1 >= c1 ) && (length d2 >= c2) = case playRec S.empty (take c1 d1, take c2 d2) of
  Left _  -> playRec (S.insert g cache) (d1 ++ [c1,c2], d2)
  Right _ -> playRec (S.insert g cache) (d1, d2 ++ [c2,c1])
playRec cache g@(c1:d1, c2:d2) | c1 > c2 = playRec (S.insert g cache) (d1 ++ [c1,c2], d2)
playRec cache g@(c1:d1, c2:d2) = playRec (S.insert g cache) (d1, d2 ++ [c2,c1])

type Card = Int
type Deck = [Int]
type Game = (Deck, Deck)

count :: Deck -> Int
count d = foldl (\s (c, idx) -> s + (c * idx)) 0 $ zip (reverse d) [1..]

example1 :: Deck
example1 = [9, 2, 6, 3, 1]
example2 :: Deck
example2 = [5, 8, 4, 7, 10]

player1 :: Deck
player1 = [
  29,
  21,
  38,
  30,
  25,
  7 ,
  2 ,
  36,
  16,
  44,
  20,
  12,
  45,
  4,
  31,
  34,
  33,
  42,
  50,
  14,
  39,
  37,
  11,
  43,
  18]

player2 :: Deck
player2 = [
  32,
  24,
  10,
  41,
  13,
  3,
  6,
  5,
  9,
  8,
  48,
  49,
  46,
  17,
  22,
  35,
  1,
  19,
  23,
  28,
  40,
  26,
  47,
  15,
  27]
