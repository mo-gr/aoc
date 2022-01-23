{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Y2015.AOC22 where

import AOC (Solution (PureSolution))
import Control.Monad (guard)
import Util (Input)

data Boss = Boss
  { hitPointsBoss :: Int,
    damage :: Int,
    poisonedFor :: Int
  }
  deriving (Eq, Show)

data Wizard = Wizard
  { hitPoints :: Int,
    mana :: Int,
    rechargingFor :: Int,
    shieldFor :: Int,
    difficulty :: Difficulty
  }
  deriving (Eq, Show)

data Difficulty = Easy | Hard
  deriving (Eq, Show)

type Fight = (Wizard, Boss)

data Spell = MagicMissileSpell | DrainSpell | ShieldSpell | PoisonSpell | RechargeSpell
  deriving (Eq, Show, Enum)

cost :: Spell -> Int
cost MagicMissileSpell = 53
cost DrainSpell = 73
cost ShieldSpell = 113
cost PoisonSpell = 173
cost RechargeSpell = 229

canApply :: Spell -> Fight -> Bool
canApply ShieldSpell (Wizard {shieldFor}, _) | shieldFor > 0 = False
canApply RechargeSpell (Wizard {rechargingFor}, _) | rechargingFor > 0 = False
canApply PoisonSpell (_, Boss {poisonedFor}) | poisonedFor > 0 = False
canApply sp (Wizard {mana}, _) | cost sp > mana = False
canApply _ _ = True

spells :: [Spell]
spells = enumFrom (toEnum 0)

playerNotDead, bossDead :: Fight -> Bool
playerNotDead (Wizard {..}, _) = hitPoints > 0
bossDead (_, Boss {..}) = hitPointsBoss <= 0

fight :: Int -> Fight -> [Int]
fight manaLimit = wizardAttack 0
  where
    wizardAttack, bossAttack :: Int -> Fight -> [Int]
    wizardAttack totalManaCost fighters
      | totalManaCost > manaLimit = []
      | otherwise = do
        let afterDifficulty = applyDifficulty fighters
        guard $ playerNotDead afterDifficulty
        let afterEffects = applyEffects afterDifficulty
        if bossDead afterEffects
          then pure totalManaCost
          else do
            sp <- spells
            guard $ canApply sp afterEffects
            let afterSpell = applySpell sp afterEffects
            if bossDead afterSpell
              then pure $ totalManaCost + cost sp
              else bossAttack (totalManaCost + cost sp) afterSpell
    bossAttack costAcc fighters = do
      let afterEffects = applyEffects fighters
      if bossDead afterEffects
        then pure costAcc
        else do
          let afterBossAttack = applyBoss afterEffects
          guard $ playerNotDead afterBossAttack
          wizardAttack costAcc afterBossAttack

applySpell :: Spell -> Fight -> Fight
applySpell sp@MagicMissileSpell (pl@Wizard {..}, bo@Boss {..}) = (pl {mana = mana - cost sp}, bo {hitPointsBoss = hitPointsBoss - 4})
applySpell sp@DrainSpell (pl@Wizard {..}, bo@Boss {..}) = (pl {mana = mana - cost sp, hitPoints = hitPoints + 2}, bo {hitPointsBoss = hitPointsBoss - 2})
applySpell sp@ShieldSpell (pl@Wizard {..}, bo) = (pl {mana = mana - cost sp, shieldFor = 6}, bo)
applySpell sp@PoisonSpell (pl@Wizard {..}, bo) = (pl {mana = mana - cost sp}, bo {poisonedFor = 6})
applySpell sp@RechargeSpell (pl@Wizard {..}, bo) = (pl {mana = mana - cost sp, rechargingFor = 5}, bo)

applyBoss :: Fight -> Fight
applyBoss (pl@Wizard {..}, bo@Boss {..})
  | shieldFor > 0 = let totalDamage = max 1 (damage - 7) in (pl {hitPoints = hitPoints - totalDamage}, bo)
  | otherwise = (pl {hitPoints = hitPoints - damage}, bo)

applyEffects :: Fight -> Fight
applyEffects = applyShield . applyRecharge . applyPoison
  where
    applyShield (w@Wizard {..}, b) | shieldFor > 0 = (w {shieldFor = pred shieldFor}, b)
    applyShield f = f
    applyRecharge (w@Wizard {..}, b)
      | rechargingFor > 0 =
        (w {rechargingFor = pred rechargingFor, mana = mana + 101}, b)
    applyRecharge f = f
    applyPoison (w, b@Boss {..})
      | poisonedFor > 0 =
        (w, b {poisonedFor = poisonedFor - 1, hitPointsBoss = hitPointsBoss - 3})
    applyPoison f = f

applyDifficulty :: Fight -> Fight
applyDifficulty f@(pl@Wizard {..}, bo)
  | difficulty == Hard = (pl {hitPoints = pred hitPoints}, bo)
  | otherwise = f

player, hardPlayer :: Wizard
player =
  Wizard
    { hitPoints = 50,
      mana = 500,
      rechargingFor = 0,
      shieldFor = 0,
      difficulty = Easy
    }
hardPlayer = player {difficulty = Hard}

boss :: Boss
boss =
  Boss
    { hitPointsBoss = 55,
      damage = 8,
      poisonedFor = 0
    }

-- 953
solution1 :: Input -> Int
solution1 _input = minimum $ fight 1000 (player, boss)

-- 1289
solution2 :: Input -> Int
solution2 _input = minimum $ fight 1500 (hardPlayer, boss)

solution :: Solution
solution = PureSolution solution1 953 solution2 1289
