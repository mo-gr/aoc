{-# LANGUAGE OverloadedStrings #-}

module Y2016.AOC17 (solution) where

import AOC (Solution (PureSolution))
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Util (Input, (|>))

md5HexString :: String -> String
md5HexString = show . md5 . pack 

data Coord = C1 | C2 | C3 | C4 deriving (Eq, Enum, Bounded)

instance Show Coord where
  show C1 = "1"
  show C2 = "2"
  show C3 = "3"
  show C4 = "4"

type Point = (Coord, Coord)

data Direction = DUp | DDown | DLeft | DRight

instance Show Direction where
  show DUp = "U"
  show DDown = "D"
  show DLeft = "L"
  show DRight = "R"

type Path = [Direction]

type Seed = [Char]

isOpenDoor :: Char -> Bool
isOpenDoor 'b' = True
isOpenDoor 'c' = True
isOpenDoor 'd' = True
isOpenDoor 'e' = True
isOpenDoor 'f' = True
isOpenDoor _ = False

openDoors :: Seed -> Path -> [Direction]
openDoors s p = md5HexString (s <> reverse (mconcat (fmap show p))) |> take 4 |> zip [DUp, DDown, DLeft, DRight] |> filter (isOpenDoor . snd) |> fmap fst

safeSucc, safePred :: (Eq a, Bounded a, Enum a) => a -> Maybe a
safeSucc a | a == maxBound = Nothing
safeSucc a = Just . succ $ a
safePred a | a == minBound = Nothing
safePred a = Just . pred $ a

move :: Point -> Direction -> Maybe Point
move (x, y) DUp = safePred y >>= \y' -> pure (x, y')
move (x, y) DLeft = safePred x >>= \x' -> pure (x', y)
move (x, y) DDown = safeSucc y >>= \y' -> pure (x, y')
move (x, y) DRight = safeSucc x >>= \x' -> pure (x', y)

pathToPoint :: Path -> Point
pathToPoint [] = (C1, C1)
pathToPoint (d : ds) = case move (pathToPoint ds) d of
  Nothing -> error "invalid path"
  Just p -> p

validMoves :: Seed -> Path -> [Direction]
validMoves s p =
  openDoors s p
    |> fmap (\d -> move (pathToPoint p) d >>= const (pure d))
    |> catMaybes

seed :: Seed
seed = "njfxhljp"

bfs :: Seed -> [Path] -> Path
bfs _ [] = error "no path"
bfs _ ps | (C4, C4) `elem` fmap pathToPoint ps = filter ((== (C4, C4)) . pathToPoint) ps |> head
bfs s ps = bfs s $ do
  p <- ps
  d <- validMoves s p
  pure (d : p)

dfs :: Seed -> Path -> [Path]
dfs _ p | pathToPoint p == (C4, C4) = [p]
dfs s p = case validMoves s p of
  [] -> []
  ps -> mconcat $ dfs s . (: p) <$> ps

-- DURLDRRDRD
solution1 :: Input -> String
solution1 _input =
  bfs seed [[]]
    |> fmap show
    |> mconcat
    |> reverse

-- 650
solution2 :: Input -> String
solution2 _input =
  dfs seed []
    |> fmap length
    |> sort
    |> last
    |> show

solution :: Solution
solution = PureSolution solution1 "DURLDRRDRD" solution2 "650"
