{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Y2016.AOC11 (solution) where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Util (Input)

data Element = Strontium | Plutonium | Thulium | Ruthenium | Curium | Elerium | Dilithium
  deriving (Eq, Ord)

instance Show Element where
  show Strontium = "S"
  show Plutonium = "P"
  show Thulium = "T"
  show Ruthenium = "R"
  show Curium = "C"
  show Elerium = "E"
  show Dilithium = "D"

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

setup, setup2 :: World Element
setup =
  [ S.fromList [Elevator, Generator Strontium, Microchip Strontium, Generator Plutonium, Microchip Plutonium],
    S.fromList [Generator Thulium, Generator Ruthenium, Microchip Ruthenium, Generator Curium, Microchip Curium],
    S.fromList [Microchip Thulium],
    S.fromList []
  ]
setup2 =
  [ S.fromList $ [Elevator, Generator Strontium, Microchip Strontium, Generator Plutonium, Microchip Plutonium] <> part2Parts,
    S.fromList [Generator Thulium, Generator Ruthenium, Microchip Ruthenium, Generator Curium, Microchip Curium],
    S.fromList [Microchip Thulium],
    S.fromList []
  ]

part2Parts :: [Item Element]
part2Parts = [Generator Elerium, Microchip Elerium, Generator Dilithium, Microchip Dilithium]

winState, winState2 :: World Element
winState =
  [ S.empty,
    S.empty,
    S.empty,
    S.fromList [Elevator, Generator Strontium, Microchip Strontium, Generator Plutonium, Microchip Plutonium, Generator Thulium, Microchip Thulium, Generator Ruthenium, Microchip Ruthenium, Generator Curium, Microchip Curium]
  ]
winState2 =
  [ S.empty,
    S.empty,
    S.empty,
    S.fromList $ [Elevator, Generator Strontium, Microchip Strontium, Generator Plutonium, Microchip Plutonium, Generator Thulium, Microchip Thulium, Generator Ruthenium, Microchip Ruthenium, Generator Curium, Microchip Curium] <> part2Parts
  ]

chipsAreSafe :: Ord a => Floor a -> Bool
chipsAreSafe s = foldr f True s
  where
    f Elevator safe = safe
    f (Generator _) safe = safe
    f (Microchip a) safe
      | S.member (Generator a) s = safe
      | noGenerator s = safe
      | otherwise = False

noGenerator :: Floor a -> Bool
noGenerator f = go $ S.toList f
  where
    go [] = True
    go (Generator _ : _) = False
    go (_ : rst) = go rst

countMovesTilWinDBF :: (Ord a) => Int -> S.Set (World a) -> S.Set (World a) -> S.Set (World a) -> S.Set (World a) -> Maybe Int
countMovesTilWinDBF n previousStatesB previousStatesE begin end
  | n > 100 = Nothing
  | not $ S.null (S.intersection begin end) = Just (2 * n)
  | not $ S.null (S.intersection begin previousStatesE) = Just ((2 * n) - 1)
  | not $ S.null (S.intersection end previousStatesB) = Just ((2 * n) - 1)
  | otherwise =
    countMovesTilWinDBF
      (succ n)
      (S.unions [previousStatesB, begin])
      (S.unions [previousStatesE, end])
      (S.fromList (concatMap (possibleMoves previousStatesB) (S.toList begin)))
      (S.fromList (concatMap (possibleMoves previousStatesE) (S.toList end)))

possibleMoves :: Ord a => S.Set (World a) -> World a -> [World a]
possibleMoves prev w =
  let possibleFillings = elevatorFillings <$> w
   in --  fmap (w : h,) . filter (`S.notMember` prev) . filter (all chipsAreSafe) $ (elevatorUp w possibleFillings <> elevatorDown w possibleFillings)
      filter (`S.notMember` prev) . filter (all chipsAreSafe) . filter (/= w) $ (elevatorUp w possibleFillings <> elevatorDown w possibleFillings)

elevatorUp :: (Ord a) => World a -> [[S.Set (Item a)]] -> [World a]
elevatorUp world possibleElevatorFillings = applyFillingUp $ zip world possibleElevatorFillings

elevatorDown :: (Ord a) => World a -> [[S.Set (Item a)]] -> [World a]
elevatorDown world possibleElevatorFillings = applyFillingDown $ zip world possibleElevatorFillings

applyFillingUp :: (Ord a) => [(Floor a, [S.Set (Item a)])] -> [World a]
applyFillingUp [] = [[]]
applyFillingUp [(fl, _)] = [[fl]]
applyFillingUp ((fl, []) : rst) = (fl :) <$> applyFillingUp rst
applyFillingUp ((fl, eles) : (nfl, _) : rst) =
  eles >>= \ele -> ([S.difference fl ele, S.union nfl ele] <>) <$> applyFillingUp rst

applyFillingDown :: (Ord a) => [(Floor a, [S.Set (Item a)])] -> [World a]
applyFillingDown [] = [[]]
applyFillingDown [(fl, _)] = [[fl]]
applyFillingDown ((fl, _) : (nfl, []) : rst) = (fl :) <$> applyFillingDown ((nfl, []) : rst)
applyFillingDown ((fl, _) : (nfl, eles) : rst) =
  eles >>= \ele -> ([S.union fl ele, S.difference nfl ele] <>) <$> applyFillingDown rst

elevatorFillings :: Ord a => Floor a -> [S.Set (Item a)]
elevatorFillings f
  | S.notMember Elevator f = []
  | length f == 2 = [f]
  | otherwise = S.toList . S.fromList $ do
    x <- S.toList f
    guard $ x /= Elevator
    y <- tail . dropWhile (/= x) $ S.toList f
    guard $ y /= Elevator
    S.fromList <$> [[Elevator, x, y], [Elevator, x], [Elevator, y]]

-- 37, 61
solution1, solution2 :: Input -> Int
solution1 _input =
  fromJust $ countMovesTilWinDBF 0 S.empty S.empty (S.singleton setup) (S.singleton winState)
solution2 _input =
  fromJust $ countMovesTilWinDBF 0 S.empty S.empty (S.singleton setup2) (S.singleton winState2)

solution :: Solution
solution = PureSolution solution1 37 solution2 61
