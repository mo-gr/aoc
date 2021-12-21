{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Y2021.AOC21 where

import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

inputParser :: [Int] -> Parser GameState
inputParser dice = do
  p1 <- string "Player 1 starting position: " *> number <* newline
  p2 <- string "Player 2 starting position: " *> number <* newline
  pure $ mkGame p1 p2 dice

data GameState = GameState
  { p1Pos :: Int,
    p1Points :: Int,
    p2Pos :: Int,
    p2Points :: Int,
    dice :: [Int],
    dieRolls :: Int,
    nextPlayer :: Player
  }
  deriving (Eq, Ord)

instance Show GameState where
  show GameState {..} =
    show p1Points
      ++ "/"
      ++ show p2Points

data Player = P1 | P2 deriving (Eq, Show, Ord)

mkGame :: Int -> Int -> [Int] -> GameState
mkGame s1 s2 d =
  GameState
    { p1Pos = s1,
      p2Pos = s2,
      p1Points = 0,
      p2Points = 0,
      dice = d,
      dieRolls = 0,
      nextPlayer = P1
    }

updatePos :: Int -> Int -> Int
updatePos m p = cycle [1 .. 10] !! (m + p - 1)

gameStep :: GameState -> GameState
gameStep g =
  let g' = g {dice = drop 3 $ dice g, dieRolls = 3 + dieRolls g}
      moves = sum $ take 3 $ dice g
   in case nextPlayer g of
        P1 -> g' {nextPlayer = P2, p1Pos = updatePos moves (p1Pos g), p1Points = p1Points g + updatePos moves (p1Pos g)}
        P2 -> g' {nextPlayer = P1, p2Pos = updatePos moves (p2Pos g), p2Points = p2Points g + updatePos moves (p2Pos g)}

isWon :: Int -> GameState -> Bool
isWon goal GameState {p1Points} | p1Points >= goal = True
isWon goal GameState {p2Points} | p2Points >= goal = True
isWon _ _ = False

play :: GameState -> [GameState]
play = iterate gameStep

score :: GameState -> Int
score GameState {..} | p1Points > p2Points = p2Points * dieRolls
score GameState {..} = p1Points * dieRolls

playDiracMap :: M.Map GameState Int -> M.Map GameState Int
playDiracMap gmap = M.foldlWithKey f gmap gmap
  where
    f :: M.Map GameState Int -> GameState -> Int -> M.Map GameState Int
    f acc g _c | isWon 21 g = acc
    f acc g c = diracStep g |> foldl (fi c) (M.delete g acc)
    fi :: Int -> M.Map GameState Int -> GameState -> M.Map GameState Int
    fi gc acc g = M.insertWith (+) g gc acc

playDiracMapTilStable :: Int -> M.Map GameState Int -> M.Map GameState Int
playDiracMapTilStable 0 _ = error "no stability reached"
playDiracMapTilStable n g = let g' = playDiracMap g in if g == g' then g else playDiracMapTilStable (n - 1) g'

byWinnings :: M.Map GameState Int -> (Int, Int)
byWinnings = M.foldlWithKey f (0, 0)
  where
    f :: (Int, Int) -> GameState -> Int -> (Int, Int)
    f (p1, p2) g c = if p1Points g > p2Points g then (p1 + c, p2) else (p1, p2 + c)

diracStep :: GameState -> [GameState]
diracStep g | isWon 21 g = [g]
diracStep g = do
  d1 <- [1 .. 3]
  d2 <- [1 .. 3]
  d3 <- [1 .. 3]
  pure $ gameStep (g {dice = [d1, d2, d3]})

-- 855624
solution1 :: Input -> Int
solution1 input =
  parseOrDie (inputParser (cycle [1 .. 100])) input
    |> play
    |> find (isWon 1000)
    |> fmap score
    |> fromJust

-- 187451244607486
solution2 :: Input -> Int
solution2 input =
  parseOrDie (inputParser []) input
    |> (`M.singleton` 1)
    |> playDiracMapTilStable 20
    |> byWinnings
    |> uncurry max

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 855624 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 187451244607486 . solution2 =<< input
    ]
