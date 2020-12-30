{-# LANGUAGE NamedFieldPuns #-}

module Y2020.AOC12 where

import           Data.Either            (fromRight)
import           Text.Parsec            (digit, endOfLine, many1, oneOf)
import           Text.Parsec.ByteString (Parser, parseFromFile)

type Coord = (Int,Int)

data Direction = North | East | West | South deriving (Show)
data Rotation = RLeft | RRight deriving (Show)

data Command = N {arg :: Int}
 | E {arg :: Int}
 | S {arg :: Int}
 | W {arg :: Int}
 | F {arg :: Int}
 | L {arg :: Int}
 | R {arg :: Int}
 deriving (Show)

number :: Parser Int
number = read <$> many1 digit

commandParser :: Parser Command
commandParser = do
  cmd <- oneOf "NESWFLR"
  a <- number
  _ <- endOfLine
  return $ case cmd of
    'N' -> N a
    'E' -> E a
    'S' -> S a
    'W' -> W a
    'F' -> F a
    'L' -> L a
    'R' -> R a
    _   -> error "something went wrong"

inputParser :: Parser [Command]
inputParser = many1 commandParser

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance p1 p2 =
  let dx = fst p1 - fst p2
      dy = snd p1 - snd p2
  in  abs dx + abs dy

data Ship = Ship {
  position  :: Coord,
  direction :: Direction,
  waypoint  :: Coord
} deriving (Show)

mkShip :: Ship
mkShip = Ship {position=(0,0), direction=East, waypoint=(10,1)}

moveShip :: Ship -> Command -> Ship
moveShip s@Ship{direction=North,position=(x,y)} (F dist) = s {position=(x,y+dist)}
moveShip s@Ship{direction=East,position=(x,y)} (F dist) = s {position=(x+dist,y)}
moveShip s@Ship{direction=South,position=(x,y)} (F dist) = s {position=(x,y-dist)}
moveShip s@Ship{direction=West,position=(x,y)} (F dist) = s {position=(x-dist,y)}
moveShip s@Ship{position=(x,y)} (N dist) = s {position=(x,y+dist)}
moveShip s@Ship{position=(x,y)} (E dist) = s {position=(x+dist,y)}
moveShip s@Ship{position=(x,y)} (S dist) = s {position=(x,y-dist)}
moveShip s@Ship{position=(x,y)} (W dist) = s {position=(x-dist,y)}
moveShip s@Ship{direction} (L rot) = s {direction = rotate direction RLeft rot}
moveShip s@Ship{direction} (R rot) = s {direction = rotate direction RRight rot}

rotate :: Direction -> Rotation -> Int -> Direction
rotate North RRight 90 = East
rotate East RRight 90  = South
rotate South RRight 90 = West
rotate West RRight 90  = North
rotate North RLeft 90  = West
rotate West RLeft 90   = South
rotate South RLeft 90  = East
rotate East RLeft 90   = North
rotate d dr 180        = rotate (rotate d dr 90) dr 90
rotate d dr 270        = rotate (rotate (rotate d dr 90) dr 90) dr 90
rotate _ _ _           = error "unsupported rotation"

neg :: Int -> Int
neg x = -1 * x

moveShipAndWaypoint :: Ship -> Command -> Ship
moveShipAndWaypoint s@Ship{position=(x,y), waypoint=(wx,wy)} (F dist) = s {position=(x + (dist * wx), y + (dist * wy))}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (N dist) = s{waypoint=(wx,wy+dist)}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (E dist) = s{waypoint=(wx+dist,wy)}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (S dist) = s{waypoint=(wx,wy-dist)}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (W dist) = s{waypoint=(wx-dist,wy)}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (R 90) =s{waypoint=(wy, neg wx)}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (L 90) =s{waypoint=(neg wy,wx)}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (R 180) =s{waypoint=(neg wx, neg wy)}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (L 180) =s{waypoint=(neg wx, neg wy)}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (R 270) =s{waypoint=(neg wy, wx)}
moveShipAndWaypoint s@Ship{waypoint=(wx,wy)} (L 270) =s{waypoint=(wy, neg wx)}
moveShipAndWaypoint s c = error $ "unknonw move: " ++ show c ++ " @ " ++ show s

-- 2297
solution1 :: IO Int
solution1 = do
  commands <- fromRight [] <$> parseFromFile inputParser "AOC12.input"
  let ship = mkShip
  let afterwards = foldl moveShip ship commands
  return $ manhattanDistance (0,0) (position afterwards)

-- 89984
solution2 :: IO Int
solution2 = do
  commands <- fromRight [] <$> parseFromFile inputParser "AOC12.input"
  let ship = mkShip
  let afterwards = foldl moveShipAndWaypoint ship commands
  return $ manhattanDistance (0,0) (position afterwards)
