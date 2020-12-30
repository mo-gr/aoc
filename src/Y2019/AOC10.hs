module Y2019.AOC10 (solution1, solution2) where

import           Data.Functor           (($>))
import           Data.List              (maximumBy, sortBy)
import qualified Data.Set               as Set
import           Text.Parsec            (getPosition, many, many1, skipMany,
                                         sourceColumn, sourceLine, space,
                                         string, (<|>))
import           Text.Parsec.ByteString (Parser, parseFromFile)

data Point = Point {_x :: Int, _y :: Int} deriving (Show, Eq, Ord)
type Angle = Double

asteroidParser :: Parser [Point]
asteroidParser = do
  _   <- string "#"
  pos <- getPosition
  return [Point { _x = sourceColumn pos - 2, _y = sourceLine pos - 1 }]

emptyParser :: Parser [Point]
emptyParser = string "." $> []

lineParser :: Parser [Point]
lineParser =
  concat <$> many1 (asteroidParser <|> emptyParser) <* skipMany space

calculateAngle :: Point -> Point -> Angle
--calculateAngle p1 p2 = let half_circle = 180
--                           a = floor $ (atan2 (fromIntegral $ _x p1 - _x p2) (fromIntegral $ _y p2 - _y p1)) / pi * (fromIntegral half_circle)
--                       in (half_circle + a) `mod` (2 * half_circle)
calculateAngle p1 p2 =
  let half_circle = 180
      a =
          atan2 (fromIntegral $ _x p1 - _x p2) (fromIntegral $ _y p2 - _y p1)
            / pi
            * half_circle
  in  toNormAngle (180 + a)

toNormAngle :: Double -> Double
toNormAngle a | a < 0    = toNormAngle (a + 360)
toNormAngle a | a >= 360 = toNormAngle (a - 360)
toNormAngle a = a

sightLines :: [Point] -> Point -> [(Angle, Point)]
sightLines [] _                    = []
sightLines (p : ps) ref | ref == p = sightLines ps ref
sightLines (p : ps) ref =
  let angle = calculateAngle ref p in (angle, p) : sightLines ps ref

countVisibles :: [(Angle, Point)] -> Int
countVisibles ps = let pSet = Set.fromList (fst <$> ps) in length pSet

manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 =
  let dx = _x p1 - _x p2
      dy = _y p1 - _y p2
  in  abs dx + abs dy

magicOrdering :: Point -> (Angle, Point) -> (Angle, Point) -> Ordering
magicOrdering origin (a1, p1) (a2, p2) | a1 == a2 =
  compare (manhattanDistance origin p1) (manhattanDistance origin p2)
magicOrdering _ (a1, _) (a2, _) = compare a1 a2

offset :: [(Angle, Point)] -> [(Angle, Point)]
offset ps = head ps : zipWith magic (tail ps) ps where
  magic (a, p) (a', _) | a == a' = (a + 360, p)
  magic (a, p) _       = (a, p)

nth :: Int -> [a] -> a
nth n l = last $ take n l

-- 276
solution1 :: IO (Point, Int)
solution1 = do
  pointsOrErrors <- parseFromFile (concat <$> many lineParser) "AOC10.input"
  case pointsOrErrors of
    Right points ->
      return
        .   maximumBy (\a b -> compare (snd a) (snd b))
        $   (\p -> (p, countVisibles (sightLines points p)))
        <$> points
    Left e -> error . show $ e

-- 1321
solution2 :: IO Int
solution2 = do
  pointsOrErrors <- parseFromFile (concat <$> many lineParser) "AOC10.input"
  case pointsOrErrors of
    Right points -> do
      let laserPosition =
            fst
              $   maximumBy (\a b -> compare (snd a) (snd b))
              $   (\p -> (p, countVisibles (sightLines points p)))
              <$> points
      let twoHundred =
            nth 200 . sortBy (magicOrdering laserPosition) . offset $ sortBy
              (magicOrdering laserPosition)
              (sightLines points laserPosition)
      return $ (100 * (_x . snd) twoHundred) + (_y . snd) twoHundred
    Left e -> error . show $ e
