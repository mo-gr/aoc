{-# LANGUAGE OverloadedStrings #-}

module Y2019.AOC3 where

import           Data.Functor           (($>))
import           Data.Maybe             (fromMaybe)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Text.Parsec            (digit, many1, sepBy1, skipMany, space,
                                         string, (<|>))
import           Text.Parsec.ByteString (Parser, parseFromFile)

data Point = Point {_x :: Int, _y :: Int} deriving (Show, Eq, Ord)

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

data Line = Line {_direction :: Direction, _length :: Int} deriving (Show, Eq)

type Wire = [Line]

number :: Parser Int
number = read <$> many1 digit

directionP :: Parser Direction
directionP =
  string "R"
    $>  RIGHT
    <|> string "L"
    $>  LEFT
    <|> string "U"
    $>  UP
    <|> string "D"
    $>  DOWN

lineP :: Parser Line
lineP = do
  dir <- directionP
  len <- number
  return $ Line { _direction = dir, _length = len }

wireP :: Parser Wire
wireP = lineP `sepBy1` string ","

parseOp :: Parser [Wire]
parseOp = many1 (wireP <* skipMany space)

--

lineToPoints :: [Point] -> Line -> [Point]
lineToPoints [] l                    = lineToPoints [Point 0 0] l
lineToPoints ps Line { _length = 0 } = ps
lineToPoints ps@(p : _) l@Line { _direction = dir } =
  lineToPoints (nextPoint p dir : ps) (shorten l)

nextPoint :: Point -> Direction -> Point
nextPoint Point { _x = x, _y = y } UP    = Point { _x = x, _y = y + 1 }
nextPoint Point { _x = x, _y = y } DOWN  = Point { _x = x, _y = y - 1 }
nextPoint Point { _x = x, _y = y } LEFT  = Point { _x = x - 1, _y = y }
nextPoint Point { _x = x, _y = y } RIGHT = Point { _x = x + 1, _y = y }

shorten :: Line -> Line
shorten Line { _direction = d, _length = l } =
  Line { _direction = d, _length = l - 1 }

wireToPointCloud :: Wire -> Set Point
wireToPointCloud lines' = Set.fromList $ foldl lineToPoints [] lines'

crossings :: [Set Point] -> Set Point
crossings [s     ] = s
crossings (s : ss) = Set.intersection s $ crossings ss
crossings _        = error "unexpected set of points"

manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 =
  let dx = _x p1 - _x p2
      dy = _y p1 - _y p2
  in  abs dx + abs dy

manhattan0 :: Point -> Int
manhattan0 = manhattanDistance origin

findSmallestDistance :: Set Point -> Int
findSmallestDistance s =
  fromMaybe 0 $ Set.lookupMin $ Set.map manhattan0 $ Set.delete origin s

origin :: Point
origin = Point 0 0

-- 1017
solution1 :: IO Int
solution1 = do
  ops <- parseFromFile parseOp "AOC3.input"
  case ops of
    Left e -> error . show $ e
    Right wires ->
      return . findSmallestDistance . crossings $ wireToPointCloud <$> wires


solution2 :: IO ()
solution2 = error "no solution yet"
