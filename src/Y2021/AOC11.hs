module Y2021.AOC11 where

import AOC (Solution (PureSolution))
import Data.Char (digitToInt)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Text.Parsec (digit, many, many1, newline)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, times, (|>))

data Octopus = Energy Int | Flash deriving (Eq)

instance Show Octopus where
  show Flash = "f"
  show (Energy n) = show n

type Cave = M.Map Point Octopus

type Point = (Int, Int)

lineParser :: Parser [Octopus]
lineParser = do
  chars <- many1 digit
  pure $ Energy . digitToInt <$> chars

inputParser :: Parser Cave
inputParser = do
  listOfLists <- many1 (lineParser <* many newline)
  pure $ M.fromList (kvPairs listOfLists)
  where
    kvPairs :: [[Octopus]] -> [(Point, Octopus)]
    kvPairs os = do
      x <- [0 .. length (head os) - 1]
      y <- [0 .. length os - 1]
      pure ((x, y), (os !! y) !! x)

neighbours :: Point -> Cave -> [Point]
neighbours (x, y) c = filter bounds $ do
  dx <- [-1 .. 1]
  dy <- [-1 .. 1]
  pure (x + dx, y + dy)
  where
    ((mx, my), _) = M.findMax c
    bounds (x', _) | x' < 0 || x' > mx = False
    bounds (_, y') | y' < 0 || y' > my = False
    bounds (x', y') | x == x' && y == y' = False
    bounds _ = True

inc :: Octopus -> Octopus
inc Flash = Flash
inc (Energy n) = Energy (n + 1)

step0 :: Cave -> (Cave, Int)
step0 c = step (c, 0)

step :: (Cave, Int) -> (Cave, Int)
step (c, fs) = M.map inc c |> flash |> fmap (+ fs)

updateAllWithKey :: (k -> a -> M.Map k a -> M.Map k a) -> M.Map k a -> M.Map k a
updateAllWithKey f c = M.foldrWithKey f c c

tagFlashed :: Cave -> (Cave, [Point])
tagFlashed c = M.foldrWithKey f (c, []) c
  where
    f p (Energy e) (c', fs) | e > 9 = (M.adjust (const Flash) p c', p : fs)
    f _ _ acc = acc

flash :: Cave -> (Cave, Int)
flash c =
  let (c', fs) = tagFlashed c
   in if null fs
        then (updateAllWithKey resetFlashed c', M.foldr countFlashed 0 c')
        else flash $ foldr f c' fs
  where
    f :: Point -> Cave -> Cave
    f p acc = flashNeighbours p ((M.!) acc p) acc

flashNeighbours :: Point -> Octopus -> Cave -> Cave
flashNeighbours p Flash cave = updateNeighbours p cave
flashNeighbours _p _octopus cave = cave

resetFlashed :: Point -> Octopus -> Cave -> Cave
resetFlashed p Flash c = M.adjust (const (Energy 0)) p c
resetFlashed _p _o c = c

countFlashed :: Octopus -> Int -> Int
countFlashed Flash f = f + 1
countFlashed _o f = f

updateNeighbours :: Point -> Cave -> Cave
updateNeighbours p cave = let ns = neighbours p cave in foldr (M.adjust inc) cave ns

linesAt :: Int -> String -> [String]
linesAt _n [] = []
linesAt n s = take n s : linesAt n (drop n s)

switch :: ((a, a), b) -> ((a, a), b)
switch ((a, a'), b) = ((a', a), b)

pretty :: Cave -> IO ()
pretty c = M.toList c |> fmap switch |> sortOn fst |> fmap snd |> fmap show |> mconcat |> linesAt (maxL + 1) |> fmap putStrLn |> mconcat
  where
    ((maxL, _), _) = M.findMax c

countTilAllZero :: Int -> Cave -> Int
countTilAllZero 10000 _c = error "run away"
countTilAllZero n c = if M.toList c |> fmap snd |> all (Energy 0 ==) then n else countTilAllZero (n + 1) (fst (step0 c))

-- 1694
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> step0
    |> times 99 step
    |> snd

-- 346
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> countTilAllZero 0

solution :: Solution
solution = PureSolution solution1 1694 solution2 346
