{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Y2021.AOC21 where

import Data.List (find, span)
import Data.Maybe (fromJust)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import Data.Bifunctor (bimap)

data GameState = GameState
  { p1Pos :: [Int],
    p1Points :: Int,
    p2Pos :: [Int],
    p2Points :: Int,
    dice :: [Int],
    dieRolls :: Int,
    nextPlayer :: Player
  }
  deriving (Eq)

instance Show GameState where
  show GameState {..} =
    (show p1Points)
      ++ "/"
      ++ (show p2Points)
      ++ " ("
      ++ (show (head p1Pos))
      ++ ":"
      ++ (show (head p2Pos))
      ++ ")"

data Player = P1 | P2 deriving (Eq, Show)

mkGame :: Int -> Int -> [Int] -> GameState
mkGame s1 s2 d =
  GameState
    { p1Pos = drop (s1 - 1) $ cycle [1 .. 10],
      p2Pos = drop (s2 - 1) $ cycle [1 .. 10],
      p1Points = 0,
      p2Points = 0,
      dice = d,
      dieRolls = 0,
      nextPlayer = P1
    }

gameStep :: GameState -> GameState
gameStep g =
  let g' = g {dice = drop 3 $ dice g, dieRolls = 3 + dieRolls g}
      moves = sum $ take 3 $ dice g
   in case nextPlayer g of
        P1 -> g' {nextPlayer = P2, p1Pos = drop moves (p1Pos g), p1Points = p1Points g + (p1Pos g) !! moves}
        P2 -> g' {nextPlayer = P1, p2Pos = drop moves (p2Pos g), p2Points = p2Points g + (p2Pos g) !! moves}

isWon :: GameState -> Bool
isWon GameState {p1Points} | p1Points >= 1000 = True
isWon GameState {p2Points} | p2Points >= 1000 = True
isWon _ = False

isEnd :: GameState -> Bool
isEnd GameState {p1Points} | p1Points >= 21 = True
isEnd GameState {p2Points} | p2Points >= 21 = True
isEnd _ = False

play :: GameState -> [GameState]
play = iterate gameStep

score :: GameState -> Int
score GameState {..} | p1Points > p2Points = p2Points * dieRolls
score GameState {..} = p1Points * dieRolls

inputParser :: Parser [String]
inputParser = undefined

playDirac :: [GameState] -> [GameState]
playDirac gs = fmap diracStep gs
  |> mconcat
  |> \gs' -> if length gs' == length gs 
             then gs'
             else playDirac gs'

splitP :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
splitP _p [] r = r
splitP p (a:as) (n, y) = if p a then splitP p as (n, a:y) else splitP p as (a:n, y)

byWinnings :: [GameState] -> (Int, Int)
byWinnings g = splitP (\g -> p1Points g > p2Points g) g ([],[])
 |> (bimap length length)

diracStep :: GameState -> [GameState]
diracStep g | isEnd g = [g]
diracStep g =
  [ gameStep (g {dice = [1, 1, 1]}),
    gameStep (g {dice = [1, 1, 2]}),
    gameStep (g {dice = [1, 1, 3]}),
    gameStep (g {dice = [1, 2, 1]}),
    gameStep (g {dice = [1, 2, 2]}),
    gameStep (g {dice = [1, 2, 3]}),
    gameStep (g {dice = [1, 3, 1]}),
    gameStep (g {dice = [1, 3, 2]}),
    gameStep (g {dice = [1, 3, 3]}),
    gameStep (g {dice = [2, 1, 1]}),
    gameStep (g {dice = [2, 1, 2]}),
    gameStep (g {dice = [2, 1, 3]}),
    gameStep (g {dice = [2, 2, 1]}),
    gameStep (g {dice = [2, 2, 2]}),
    gameStep (g {dice = [2, 2, 3]}),
    gameStep (g {dice = [2, 3, 1]}),
    gameStep (g {dice = [2, 3, 2]}),
    gameStep (g {dice = [2, 3, 3]}),
    gameStep (g {dice = [3, 1, 1]}),
    gameStep (g {dice = [3, 1, 2]}),
    gameStep (g {dice = [3, 1, 3]}),
    gameStep (g {dice = [3, 2, 1]}),
    gameStep (g {dice = [3, 2, 2]}),
    gameStep (g {dice = [3, 2, 3]}),
    gameStep (g {dice = [3, 3, 1]}),
    gameStep (g {dice = [3, 3, 2]}),
    gameStep (g {dice = [3, 3, 3]})
  ]

-- mkGame 4 8 (cycle [1,2,3])

-- 855624
solution1 :: Input -> Int
solution1 _input = mkGame 4 10 (cycle [1 .. 100]) |> play |> find isWon |> fmap score |> fromJust

-- parseOrDie inputParser input
--  |> error "not yet"

maxPair :: (Int, Int) -> Int
maxPair (x,y) | x> y = x
maxPair (_,y) = y

solution2 :: Input -> Int
solution2 _input = mkGame 4 10 [] |> (:[]) |> playDirac |> byWinnings |> maxPair
  --parseOrDie inputParser input
    -- |> error "not yet"

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 855624 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" undefined . solution2 =<< input
    ]

testData :: Input
testData = ""
