{-# LANGUAGE NamedFieldPuns #-}

module Y2019.AOC14
  ( solution1,
    solution2,
  )
where

import qualified Data.Map as M
import Text.Parsec (letter, many1, newline, sepBy, string)
import Text.Parsec.ByteString (Parser, parseFromFile)
import Util (number)

type Count = Int

newtype Chemical = Chemical
  { chemicalName :: String
  }
  deriving (Show, Eq, Ord)

data Reaction = Reaction
  { input :: [(Count, Chemical)],
    output :: (Count, Chemical)
  }
  deriving (Show)

reactionParser :: Parser Reaction
reactionParser = do
  input' <- sepBy ((,) <$> countParser <* string " " <*> chemicalParser) (string ", ")
  _ <- string " => "
  output' <- (,) <$> countParser <* string " " <*> chemicalParser
  return $ Reaction input' output'
  where
    countParser :: Parser Count
    countParser = number
    chemicalParser :: Parser Chemical
    chemicalParser = Chemical <$> many1 letter

findReaction :: [Reaction] -> Chemical -> Reaction
findReaction [] _ = error "no reaction found"
findReaction (r : rs) c
  | snd (output r) == c = r
  | otherwise = findReaction rs c

fuelBom :: [Reaction] -> M.Map Chemical Count -> M.Map Chemical Count
fuelBom _rs bom | onlyOre bom = bom
fuelBom rs bom =
  let (chem, cnt) = head $ filter positiveBom $ filter notOre $ M.assocs bom
      Reaction {input, output} = findReaction rs chem
      yield = fst output
      times = if yield >= cnt then 1 else (cnt `div` yield) + (if cnt `mod` yield == 0 then 0 else 1)
   in fuelBom rs $ M.adjust (\c -> c - (times * yield)) chem $ foldl (\acc (cnt', chem') -> M.insertWith (+) chem' (times * cnt') acc) bom input

notOre :: (Chemical, b) -> Bool
notOre = (/= Chemical "ORE") . fst

positiveBom :: (Chemical, Count) -> Bool
positiveBom = (> 0) . snd

onlyOre :: M.Map Chemical Count -> Bool
onlyOre bom = all oreOrNeg $ M.assocs bom
  where
    oreOrNeg (Chemical "ORE", _) = True
    oreOrNeg (_, x) | x <= 0 = True
    oreOrNeg _ = False

-- 374457
solution1 :: IO ()
solution1 = do
  Right ops <- parseFromFile (reactionParser `sepBy` newline) "AOC14.input"
  print $ M.lookup (Chemical "ORE") $ fuelBom ops $ M.singleton (Chemical "FUEL") 1

oreInput :: Int
oreInput = 1000000000000

maxFuel :: [Reaction] -> Int -> Int
maxFuel ops fuel = case M.lookup (Chemical "ORE") $ fuelBom ops $ M.singleton (Chemical "FUEL") fuel of
  Nothing -> error "something went wrong"
  Just oreCost -> if oreCost > oreInput then fuel - 1 else maxFuel ops (fuel + 1)

-- 3568888
solution2 :: IO ()
solution2 = do
  Right ops <- parseFromFile (reactionParser `sepBy` newline) "AOC14.input"
  print $ maxFuel ops 3560000 -- determined by manual binary search
