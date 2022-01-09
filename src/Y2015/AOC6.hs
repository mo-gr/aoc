module Y2015.AOC6 where

import AOC (Solution (PureSolution))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (many1, newline, string, try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data Op = TurnOn Range | TurnOff Range | Toggle Range
  deriving (Show, Eq)

type Range = (Point, Point)

type Point = (Int, Int)

data Light = On | Off | Dim Int
  deriving (Show, Eq)

type Lights = M.Map Point Light

mkLights :: Lights
mkLights = M.empty

opParser :: Parser Op
opParser = do
  op <-
    try (string "turn on " $> TurnOn)
      <|> try (string "turn off " $> TurnOff)
      <|> (string "toggle " $> Toggle)
  x <- number <* string ","
  y <- number <* string " through "
  x' <- number <* string ","
  y' <- number <* newline
  pure $ op ((x, y), (x', y'))

inputParser :: Parser [Op]
inputParser = many1 opParser

pointsInOp :: Op -> [Point]
pointsInOp (TurnOn r) = pointsInRange r
pointsInOp (TurnOff r) = pointsInRange r
pointsInOp (Toggle r) = pointsInRange r

pointsInRange :: Range -> [Point]
pointsInRange ((x, y), (x', y')) = do
  py <- [y .. y']
  px <- [x .. x']
  pure (px, py)

eval :: Lights -> Op -> Lights
eval lights o = foldl (f o) lights (pointsInOp o)
  where
    f (TurnOn _) ls l = M.insert l On ls
    f (TurnOff _) ls l = M.insert l Off ls
    f (Toggle _) ls l | M.notMember l ls = M.insert l On ls
    f (Toggle _) ls l = M.adjust toggle l ls
    toggle On = Off
    toggle Off = On
    toggle _ = error "something went wrong"

evalDim :: Lights -> Op -> Lights
evalDim lights o = foldl (f o) lights (pointsInOp o)
  where
    f (TurnOn _) ls l = M.insertWith (dim 1) l (Dim 1) ls
    f (TurnOff _) ls l = M.insertWith (dim (-1)) l (Dim 0) ls
    f (Toggle _) ls l = M.insertWith (dim 2) l (Dim 2) ls
    dim i _new (Dim d) = Dim $ max 0 (d + i)
    dim _ _ _ = error "dimming non dimmable light"

lightVal :: Light -> Int
lightVal (Dim d) = d
lightVal _ = error "lightval of non-dimmable light"

-- 569999
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> foldl eval mkLights
    |> M.elems
    |> filter (== On)
    |> length

-- 17836115
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> foldl evalDim mkLights
    |> M.elems
    |> fmap lightVal
    |> sum

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 569999 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 17836115 . solution2 =<< input
    ]

pretty :: Lights -> IO ()
pretty ls = sequence_ $ do
  y <- [0 .. 5]
  x <- [0 .. 5]
  let prnt = if x == 5 then putStrLn else putStr
  pure $ if M.lookup (x, y) ls == Just On then prnt "#" else prnt "."

solution :: Solution
solution = PureSolution solution1 solution2 verify
