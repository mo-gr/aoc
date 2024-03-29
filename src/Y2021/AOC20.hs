module Y2021.AOC20 where

import AOC (Solution (PureSolution))
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Text.Parsec (char, many1, newline, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, times, (|>))

data Bit = O | I deriving (Eq)

instance Show Bit where
  show O = "."
  show I = "#"

type Algorithm = [Bit]

type Point = (Int, Int)

type Image = M.Map Point Bit

algorithmParser :: Parser Algorithm
algorithmParser = many1 ((char '.' $> O) <|> (char '#' $> I)) <* newline

imageParser :: Parser Image
imageParser = do
  listOfLists <- many1 (many1 ((char '.' $> O) <|> (char '#' $> I)) <* newline)
  pure $ M.fromList (kvPairs listOfLists)
  where
    kvPairs :: [[Bit]] -> [(Point, Bit)]
    kvPairs os = do
      x <- [0 .. length (head os) - 1]
      y <- [0 .. length os - 1]
      pure ((x, y), (os !! y) !! x)

inputParser :: Parser (Algorithm, Image)
inputParser = do
  a <- algorithmParser
  _ <- newline
  i <- imageParser
  pure (a, i)

linesAt :: Int -> String -> [String]
linesAt _n [] = []
linesAt n s = take n s : linesAt n (drop n s)

switch :: ((a, a), b) -> ((a, a), b)
switch ((a, a'), b) = ((a', a), b)

printImage :: Bit -> Image -> IO ()
printImage infinityDefault im =
  let (xMin, xMax) = (M.keys im |> fmap fst |> minimum, M.keys im |> fmap fst |> maximum)
      (yMin, yMax) = (M.keys im |> fmap snd |> minimum, M.keys im |> fmap snd |> maximum)
      points = do
        y <- [(yMin - 1) .. (yMax + 1)]
        x <- [xMin .. xMax]
        [(x, y)]
   in mconcat (points |> fmap (\p -> if fst p == xMax then print (getPixel im infinityDefault p) else putStr (show $ getPixel im infinityDefault p)))

neighbours :: Point -> [Point]
neighbours (x, y) = do
  dy <- [-1 .. 1]
  dx <- [-1 .. 1]
  pure (x + dx, y + dy)

insertAllNeighboursOfOne :: Image -> Image
insertAllNeighboursOfOne im = M.foldrWithKey f im im
  where
    f :: Point -> Bit -> Image -> Image
    f _ O i = i
    f p I i = neighbours p |> filter (`M.notMember` i) |> insertZeros i
    insertZeros :: Image -> [Point] -> Image
    insertZeros m ps = foldl (\macc p -> M.insert p O macc) m ps

getPixel :: Image -> Bit -> Point -> Bit
getPixel im infinityDefault p = M.findWithDefault infinityDefault p im

bitsToNumber :: [Bit] -> Int
bitsToNumber [] = 0
bitsToNumber (I : bs) = (2 ^ length bs) + bitsToNumber bs
bitsToNumber (_ : bs) = bitsToNumber bs

getEnhancedPixel :: Algorithm -> Image -> Point -> Bit -> Bit
getEnhancedPixel al im p infinityDefault =
  let bits :: [Bit]
      bits = neighbours p |> fmap (getPixel im infinityDefault)
      index = bitsToNumber bits
   in al !! index

nextDefault :: Algorithm -> Bit -> Bit
nextDefault al I = al !! 511
nextDefault al O = head al

enhance :: Algorithm -> Bit -> Image -> (Bit, Image)
enhance al infinityDefault origIm =
  let im = origIm
      (xMin, xMax) = (M.keys im |> fmap fst |> minimum, M.keys im |> fmap fst |> maximum)
      (yMin, yMax) = (M.keys im |> fmap snd |> minimum, M.keys im |> fmap snd |> maximum)
      points = do
        y <- [(yMin - 3) .. (yMax + 3)]
        x <- [(xMin - 3) .. (xMax + 3)]
        [(x, y)]
   in (nextDefault al infinityDefault, M.fromList $ fmap (\p -> (p, getEnhancedPixel al im p infinityDefault)) points)

countPixel :: Image -> Int
countPixel = M.foldl f 0
  where
    f acc O = acc
    f acc I = acc + 1

runSolution :: (Algorithm, Image) -> Int
runSolution (al, im) =
  enhance al O im
    |> uncurry (enhance al)
    |> snd
    |> countPixel

runSolution2 :: (Algorithm, Image) -> Int
runSolution2 (al, im) =
  (O, im)
    |> times 50 (uncurry (enhance al))
    |> snd
    |> countPixel

-- 5419
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> runSolution

-- 17325
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> runSolution2

solution :: Solution
solution = PureSolution solution1 5419 solution2 17325
