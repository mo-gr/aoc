{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Y2015.AOC21 where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import Data.List (subsequences)
import Util (Input)

data Fighter = Fighter
  { hitPoints :: Int,
    totalDamage :: Int,
    totalArmor :: Int
  }
  deriving (Show, Eq)

data Item = Item
  { name :: String,
    cost :: Int,
    damage :: Int,
    armor :: Int
  }
  deriving (Eq)

instance Show Item where
  show Item {name} = name

type Arena = (Fighter, Fighter)

data Result = Win | Lose deriving (Eq, Show)

attack :: Arena -> Arena
attack (attacker@Fighter {totalDamage}, defender@Fighter {totalArmor, hitPoints}) =
  let attackDamage = max 1 $ totalDamage - totalArmor
   in (attacker, defender {hitPoints = hitPoints - attackDamage})

equip :: Fighter -> Item -> Fighter
equip f@Fighter {totalDamage, totalArmor} Item {damage, armor} =
  f {totalArmor = totalArmor + armor, totalDamage = totalDamage + damage}

fight :: Arena -> Result
fight (p1, p2) = case attack (p1, p2) of
  (_, p2') | hitPoints p2' <= 0 -> Win
  (_, p2') -> case attack (p2', p1) of
    (_, p1') | hitPoints p1' <= 0 -> Lose
    (_, p1') -> fight (p1', p2')

pickOne, pickZeroOrOne, pickUpToTwo :: [a] -> [[a]]
pickOne = fmap pure
pickZeroOrOne items = [[]] <> fmap pure items
pickUpToTwo items = filter ((< 3) . length) $ subsequences items

scenarioCosts :: (Result -> Bool) -> [Int]
scenarioCosts predicate = do
  w <- pickOne weapons
  a <- pickZeroOrOne armors
  rs <- pickUpToTwo rings
  let items = w <> a <> rs
  let playerWithItems = foldl equip player items
  guard . predicate $ fight (playerWithItems, boss)
  pure . sum $ fmap cost items

boss :: Fighter
boss =
  Fighter
    { hitPoints = 109,
      totalDamage = 8,
      totalArmor = 2
    }

player :: Fighter
player =
  Fighter
    { hitPoints = 100,
      totalDamage = 0,
      totalArmor = 0
    }

weapons :: [Item]
weapons =
  [ Item "Dagger" 8 4 0,
    Item "Shortsword" 10 5 0,
    Item "Warhammer" 25 6 0,
    Item "Longsword" 40 7 0,
    Item "Greataxe" 74 8 0
  ]

armors :: [Item]
armors =
  [ Item "Leather" 13 0 1,
    Item "Chainmail" 31 0 2,
    Item "Splintmail" 53 0 3,
    Item "Bandedmail" 75 0 4,
    Item "Platemail" 102 0 5
  ]

rings :: [Item]
rings =
  [ Item "Damage +1" 25 1 0,
    Item "Damage +2" 50 2 0,
    Item "Damage +3" 100 3 0,
    Item "Defense +1" 20 0 1,
    Item "Defense +2" 40 0 2,
    Item "Defense +3" 80 0 3
  ]

-- 111
solution1 :: Input -> Int
solution1 _input = minimum $ scenarioCosts (== Win)

-- 188
solution2 :: Input -> Int
solution2 _input = maximum $ scenarioCosts (== Lose)

solution :: Solution
solution = PureSolution solution1 111 solution2 188
