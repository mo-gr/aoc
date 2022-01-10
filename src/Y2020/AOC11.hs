module Y2020.AOC11 where

import AOC (Solution (PureSolution))
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Text.Parsec (char, endOfLine, many1, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

type World = [[Layout]]

data Layout = Floor | Occupied | Empty deriving (Eq)

instance Show Layout where
  show Floor = "."
  show Occupied = "#"
  show Empty = "L"

type Coord = (Int, Int)

coords :: [[Layout]] -> [Coord]
coords ll@(l : _ll) = do
  y <- [0 .. (length ll -1)]
  x <- [0 .. (length l - 1)]
  return (x, y)
coords _ = []

neighbours :: Coord -> [Coord]
neighbours (x, y) = filter (/= (x, y)) $ do
  x' <- [x -1 .. x + 1]
  y' <- [y -1 .. y + 1]
  return (x', y')

neighbours' :: World -> Coord -> [Layout]
neighbours' w c = catMaybes $ getAt w <$> neighbours c

visibleNeighbours :: World -> Coord -> [Layout]
visibleNeighbours w c = catMaybes $ (\d -> visible w d c) <$> dirs

data Dir = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq)

dirs :: [Dir]
dirs = [N, NE, E, SE, S, SW, W, NW]

visible :: World -> Dir -> Coord -> Maybe Layout
visible w N (x, y) =
  let c' = (x, y -1)
   in case getAt w c' of
        Just Floor -> visible w N c'
        v -> v
visible w NE (x, y) =
  let c' = (x + 1, y -1)
   in case getAt w c' of
        Just Floor -> visible w NE c'
        v -> v
visible w E (x, y) =
  let c' = (x + 1, y)
   in case getAt w c' of
        Just Floor -> visible w E c'
        v -> v
visible w SE (x, y) =
  let c' = (x + 1, y + 1)
   in case getAt w c' of
        Just Floor -> visible w SE c'
        v -> v
visible w S (x, y) =
  let c' = (x, y + 1)
   in case getAt w c' of
        Just Floor -> visible w S c'
        v -> v
visible w SW (x, y) =
  let c' = (x -1, y + 1)
   in case getAt w c' of
        Just Floor -> visible w SW c'
        v -> v
visible w W (x, y) =
  let c' = (x -1, y)
   in case getAt w c' of
        Just Floor -> visible w W c'
        v -> v
visible w NW (x, y) =
  let c' = (x -1, y -1)
   in case getAt w c' of
        Just Floor -> visible w NW c'
        v -> v

atIndex :: Int -> [a] -> Maybe a
atIndex _ [] = Nothing
atIndex 0 (x : _) = Just x
atIndex i (_ : xs) = atIndex (i - 1) xs

getAt :: World -> Coord -> Maybe Layout
getAt ll (x, y) = case atIndex y ll of
  Just l -> atIndex x l
  Nothing -> Nothing

layoutParser :: Parser Layout
layoutParser = char '.' $> Floor <|> char 'L' $> Empty <|> char '#' $> Occupied

lineParser :: Parser [Layout]
lineParser = many1 layoutParser

inputParser :: Parser World
inputParser = many1 (lineParser <* endOfLine)

count :: Layout -> [Layout] -> Int
count _l [] = 0
count l (l' : ll) | l == l' = 1 + count l ll
count l ll = count l (tail ll)

type TickFn = World -> Coord -> Layout

tick :: TickFn
tick w c = case getAt w c of
  Just Empty -> case count Occupied $ neighbours' w c of
    0 -> Occupied
    _ -> Empty
  Just Occupied -> if count Occupied (neighbours' w c) >= 4 then Empty else Occupied
  Just x -> x
  _ -> error "unexpected coord"

visibleTick :: TickFn
visibleTick w c = case getAt w c of
  Just Empty -> case count Occupied $ visibleNeighbours w c of
    0 -> Occupied
    _ -> Empty
  Just Occupied -> if count Occupied (visibleNeighbours w c) >= 5 then Empty else Occupied
  Just x -> x
  _ -> error "unexpected coord"

chunk :: [[a]] -> [a] -> [[a]]
chunk _w [] = []
chunk w xs = let n = (length . head $ w) in take n xs : chunk w (drop n xs)

tickWorld :: TickFn -> World -> World
tickWorld tf w = chunk w $ tf w <$> coords w

runTilStable :: TickFn -> World -> World
runTilStable tf w =
  let w' = tickWorld tf w
   in if w == w' then w else runTilStable tf w'

countOccupied :: World -> Int
countOccupied w = count Occupied $ mconcat w

-- 2354
solution1 :: Input -> Int
solution1 input = parseOrDie inputParser input |> countOccupied . runTilStable tick

-- 2072
solution2 :: Input -> Int
solution2 input = parseOrDie inputParser input |> countOccupied . runTilStable visibleTick

solution :: Solution
solution = PureSolution solution1 2354 solution2 2072
