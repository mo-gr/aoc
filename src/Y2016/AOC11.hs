{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Y2016.AOC11 where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import Data.List (intersperse, sortOn)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace
import Util (Input)
import AStar (aStar)

data Element = Strontium | Plutonium | Thulium | Ruthenium | Curium
  deriving (Eq, Ord)

instance Show Element where
  show Strontium = "S"
  show Plutonium = "P"
  show Thulium = "T"
  show Ruthenium = "R"
  show Curium = "C"

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

testSetup, testSetup3, testSetup4 :: World TestElement
testSetup =
  [ S.fromList [Elevator, Microchip Hydrogen, Microchip Lithium],
    S.fromList [Generator Hydrogen],
    S.fromList [Generator Lithium],
    S.fromList []
  ]
testSetup3 =
  [ S.fromList [Microchip Lithium],
    S.fromList [Elevator, Microchip Hydrogen],
    S.fromList [Generator Lithium, Generator Hydrogen],
    S.fromList []
  ]
testSetup4 =
  [ S.fromList [Elevator, Microchip Hydrogen, Microchip Lithium],
    S.fromList [],
    S.fromList [Generator Lithium, Generator Hydrogen],
    S.fromList []
  ]

setup :: World Element
setup =
  [ S.fromList [Elevator, Generator Strontium, Microchip Strontium, Generator Plutonium, Microchip Plutonium],
    S.fromList [Generator Thulium, Generator Ruthenium, Microchip Ruthenium, Generator Curium, Microchip Curium],
    S.fromList [Microchip Thulium],
    S.fromList []
  ]

winState :: World Element
winState =
  [ S.empty,
    S.empty,
    S.empty,
    S.fromList [Elevator, Generator Strontium, Microchip Strontium, Generator Plutonium, Microchip Plutonium, Generator Thulium, Microchip Thulium, Generator Ruthenium, Microchip Ruthenium, Generator Curium, Microchip Curium]
  ]

testWinState :: World TestElement
testWinState =
  [ S.empty,
    S.empty,
    S.empty,
    S.fromList [Elevator, Microchip Hydrogen, Microchip Lithium, Generator Hydrogen, Generator Lithium]
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

estimateCost :: World a -> Int
estimateCost [l1, l2, l3, _l4] = length l1 * 3 + length l2 * 2 + length l3
estimateCost w = error $ "invalid world: " <> showWorld w

--countMovesTilWinDF :: (Ord a) =>  Int -> World a -> S.Set (World a) -> World a -> Maybe Int
--countMovesTilWinDF n win previousStates world
--  | n > 20 = Nothing -- cutoff
--  | win == world = trace (showWorld world) $ Just n
--  | otherwise = firstJust $ countMovesTilWinDF (succ n) win (S.insert world previousStates) <$> possibleMoves previousStates world
countMovesTilWinBF :: (Ord a) => Int -> World a -> S.Set (World a) -> [World a] -> Maybe Int
countMovesTilWinBF n win previousStates worlds
  | traceShowId n > 500 = Nothing -- cutoff
  | win `elem` worlds =  Just n
  | otherwise =
    countMovesTilWinBF
      (succ n)
      win
      (S.union previousStates (S.fromList worlds))
      (onlyMostPromising (concatMap (possibleMoves previousStates) worlds))

countMovesTilWinDBF :: (Ord a) => Int -> S.Set (World a) -> S.Set (World a) -> S.Set (World a) -> S.Set (World a) -> Maybe Int
countMovesTilWinDBF n previousStatesB previousStatesE begin end
  | traceShowId n > 20 = Nothing
  | not $ S.null (S.intersection begin end) = Just (2*n)
  | not $ S.null (S.intersection begin previousStatesE) = Just ((2*n) - 1)
  | not $ S.null (S.intersection end previousStatesB) = Just ((2*n) - 1)
  | otherwise =
    countMovesTilWinDBF
      (succ n)
      (S.unions [previousStatesB, begin])
      (S.unions [previousStatesE, end])
      (S.fromList (concatMap (possibleMoves previousStatesB) (S.toList begin)))
      (S.fromList (concatMap (possibleMoves previousStatesE) (S.toList end)))

intersections :: Ord a => [S.Set a] -> S.Set a
intersections [] = S.empty
intersections [x] = x
intersections (x:rest) = S.intersection x (intersections rest)

onlyMostPromising :: [World a] -> [World a]
onlyMostPromising = take 10000 . sortOn estimateCost

countMovesTilWinAStar :: Ord a => World a -> World a -> Maybe Int
countMovesTilWinAStar win start = length <$> aStar
    (S.fromList . possibleMoves S.empty)
    (\_ _-> 1)
    estimateCost
    (== win)
    start

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust ((Just a) : _) = Just a
firstJust (_ : rst) = firstJust rst

showWorld :: World a -> String
showWorld w = mconcat $ reverse . intersperse "\n" $ fmap (show . S.toList) w

possibleMoves :: Ord a => S.Set (World a) -> World a -> [World a]
possibleMoves prev w =
  let possibleFillings = elevatorFillings <$> w
   in --  fmap (w : h,) . filter (`S.notMember` prev) . filter (all chipsAreSafe) $ (elevatorUp w possibleFillings <> elevatorDown w possibleFillings)
      filter (`S.notMember` prev) . filter (all chipsAreSafe) .filter (/= w) $ (elevatorUp w possibleFillings <> elevatorDown w possibleFillings)

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

-- not 42
solution1, solution2 :: Input -> Int
solution2 _input =
  --fromJust $ countMovesTilWinBF 0 winState S.empty [([], setup)]
--  fromJust $ countMovesTilWinBF 0 setup S.empty [winState]
  fromJust $ countMovesTilWinDBF 0 S.empty S.empty (S.singleton setup) (S.singleton winState)
solution1 _input =
--  fromJust $ countMovesTilWinBF 0 testWinState S.empty [([], testSetup)]
--  fromJust $ countMovesTilWinBF 0 testSetup S.empty [testWinState]
  fromJust $ countMovesTilWinDBF 0 S.empty S.empty (S.singleton testSetup) (S.singleton testWinState)

solution :: Solution
solution = PureSolution solution1 37 solution2 undefined
