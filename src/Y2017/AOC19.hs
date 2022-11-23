{-# LANGUAGE OverloadedStrings #-}

module Y2017.AOC19 where

import AOC (Solution (PureSolution))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))
import Text.Parsec (char, sourceLine, sourceColumn, statePos, getParserState, letter, space, many, try)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Debug.Trace (traceShow)
import Data.Maybe (fromJust)

data Field = Path | Corner | Letter Char
  deriving (Show, Eq)
  
type Point = (Int, Int)
type World = M.Map Point Field

data Direction = North | South | West | East
  deriving (Show, Eq)

inputParser :: Parser [(Point, Field)]
inputParser = many $ try (many space *> (letterParser <|> cornerParser <|> pathParser))

letterParser, cornerParser, pathParser :: Parser (Point, Field)
letterParser = do
  sp <- statePos <$> getParserState
  p <- Letter <$> letter
  pure ((sourceColumn sp, sourceLine sp), p)
cornerParser = do
  sp <- statePos <$> getParserState
  p <- char '+' $> Corner
  pure ((sourceColumn sp, sourceLine sp), p)   
pathParser = do
  sp <- statePos <$> getParserState
  p <- (char '|' <|> char '-') $> Path
  pure ((sourceColumn sp, sourceLine sp), p)    

next :: Point -> Direction -> Point
next (x,y) North = (x, pred y)
next (x,y) South = (x, succ y)
next (x,y) East = (succ x, y)
next (x,y) West = (pred x, y)

turnTo :: World -> Point -> Direction -> Direction
turnTo w (x,y) South = fromJust $ (M.lookup (succ x, y) w >> pure East) <|> (M.lookup (pred x, y) w >> pure West)
turnTo w (x,y) North = fromJust $ (M.lookup (succ x, y) w >> pure East) <|> (M.lookup (pred x, y) w >> pure West)
turnTo w (x,y) West = fromJust $ (M.lookup (x, pred y) w >> pure North) <|> (M.lookup (x, succ y) w >> pure South)
turnTo w (x,y) East = fromJust $ (M.lookup (x, pred y) w >> pure North) <|> (M.lookup (x, succ y) w >> pure South)

walkTilCorner :: World -> Point -> Direction -> (Point, Direction)
walkTilCorner w start dir = case M.lookup (next start dir) w of
  Nothing -> error "broken path"
  Just Path -> walkTilCorner w (next start dir) dir
  Just (Letter c) -> traceShow c (walkTilCorner w (next start dir) dir)
  Just Corner -> (next start dir, turnTo w (next start dir) dir) 

findStart :: World -> Point
findStart w = firstJust $ (\x -> M.lookup (x,1) w >> pure (x,1)) <$> [1..]
  where firstJust :: [Maybe a] -> a
        firstJust ((Just x):_) = x 
        firstJust (Nothing:rest) = firstJust rest
        firstJust [] = error "no justice in this world"   

solve1 :: World -> (Point, Direction)
solve1 w = let start = findStart w 
  in walk w (start, South)

walk :: World -> (Point, Direction) -> (Point, Direction)
walk w (start, dir) = walk w (walkTilCorner w start dir) 

-- XYFDJNRCQA
solution1 :: Input -> String
solution1 input =
  parseOrDie inputParser input
    |> M.fromList
    |> solve1
    |> show

solution2 :: Input -> String
solution2 input =
  parseOrDie inputParser input
    |> undefined

solution :: Solution
solution = PureSolution solution1 "XYFDJNRCQA" solution2 undefined

testData :: Input
testData = "  |  \na |  \n  +-A+\n     |\n"