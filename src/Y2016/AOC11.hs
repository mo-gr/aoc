{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Y2016.AOC11 where

import Util (Input)
import AOC (Solution(PureSolution))
import qualified Data.Set as S
import Control.Monad (guard)
import Data.Maybe (fromJust)
import Data.List (intersperse)
import Debug.Trace

data TestElement = Hydrogen | Lithium
  deriving (Eq, Ord)

instance Show TestElement where
  show Hydrogen = "H"
  show Lithium = "L"

data Item a where
  Microchip :: Show a => a -> Item a
  Generator :: Show a => a -> Item a
  Elevator :: Item a

deriving instance Ord a => Ord (Item a)
deriving instance Eq a => Eq (Item a)
instance Show (Item a) where
  show Elevator = "E"
  show (Microchip e) = "M" <> show e
  show (Generator e) = "G" <> show e

type Floor a = S.Set (Item a)
type World a = [Floor a]

testSetup :: World TestElement
testSetup = [
  S.fromList [Elevator, Microchip Hydrogen, Microchip Lithium],
  S.fromList [Generator Hydrogen],
  S.fromList [Generator Lithium],
  S.fromList []
  ]

winState :: World TestElement
winState = [
  S.empty,
  S.empty,
  S.empty,
  S.fromList [Elevator, Microchip Hydrogen, Microchip Lithium, Generator Hydrogen, Generator Lithium]
  ]

chipsAreSafe :: Ord a => Floor a -> Bool
chipsAreSafe s = foldr f True s
  where f Elevator safe = safe
        f (Generator _) safe = safe
        f (Microchip a) safe | S.member (Generator a) s = safe
                             | otherwise = False

countMovesTilWinDF :: (Ord a) =>  Int -> World a -> S.Set (World a) -> World a -> Maybe Int
countMovesTilWinDF n win previousStates world
  | n > 20 = Nothing -- cutoff
  | win == world = trace (showWorld world) $ Just n
  | otherwise = firstJust $ countMovesTilWinDF (succ n) win (S.insert world previousStates) <$> possibleMoves previousStates world
countMovesTilWinBF :: (Ord a) => Int -> World a -> S.Set (World a) -> [World a] -> Maybe Int
countMovesTilWinBF n win previousStates worlds
  | n > 20 = Nothing -- cutoff
  | win `elem` worlds = trace (showWorld.head.filter(== win) $ worlds) $ Just n
  | otherwise = countMovesTilWinBF (succ n) win (S.union previousStates (S.fromList worlds)) (concatMap (possibleMoves previousStates) worlds)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust( (Just a):_) = Just a
firstJust (_:rst) = firstJust rst

showWorld :: World a -> String
showWorld w = mconcat $ intersperse "\n" $ fmap (show.S.toList) w

possibleMoves :: Ord a => S.Set (World a) -> World a -> [World a]
possibleMoves _ w | length w /= 4 = error $ "invalid world:\n" <> showWorld w
possibleMoves prev w = let possibleFillings = elevatorFillings <$> w in
  filter (`S.notMember` prev) $ elevatorUp w possibleFillings <> elevatorDown w possibleFillings

elevatorUp :: (Ord a) => World a -> [[S.Set (Item a)]] -> [World a]
elevatorUp world possibleElevatorFillings = applyFillingUp $ zip world possibleElevatorFillings

elevatorDown :: (Ord a) => World a -> [[S.Set (Item a)]] -> [World a]
elevatorDown world possibleElevatorFillings = applyFillingDown $ zip world possibleElevatorFillings


applyFillingUp :: (Ord a) => [(Floor a, [S.Set (Item a)])] -> [World a]
applyFillingUp [] = [[]]
applyFillingUp [(fl, _)] = [[fl]]
applyFillingUp ((fl, []):rst) = (fl:) <$> applyFillingUp rst
applyFillingUp ((fl, eles):(nfl, _):rst) =
  eles >>= \ele -> ([S.difference fl ele, S.union nfl ele] <>) <$> applyFillingUp rst

applyFillingDown :: (Ord a) => [(Floor a, [S.Set (Item a)])] -> [World a]
applyFillingDown [] = [[]]
applyFillingDown [(fl,_)] = [[fl]]
applyFillingDown ((fl, _):(nfl, []):rst) = (fl:) <$> applyFillingDown ((nfl, []):rst)
applyFillingDown ((fl, _):(nfl, eles):rst) =
  eles >>= \ele -> ([S.union fl ele, S.difference nfl ele] <>) <$> applyFillingDown rst

elevatorFillings :: Ord a => Floor a -> [S.Set (Item a)]
elevatorFillings f | S.notMember Elevator f = []
  | otherwise = S.toList . S.fromList $ do
    x <- S.toList f
    guard $ x /= Elevator
    y <- tail . dropWhile (/= x) $ S.toList f
    guard $ y /= Elevator
    S.fromList <$> [[Elevator, x],[Elevator, y],[Elevator, x,y]]

solution1 :: Input -> Int
solution1 _input =
    fromJust $ countMovesTilWinBF 0 winState S.empty [testSetup]

solution2 :: Input -> Int
solution2 _input =
   fromJust $ countMovesTilWinDF 0 winState S.empty testSetup

solution :: Solution
solution = PureSolution solution1 undefined solution2 undefined