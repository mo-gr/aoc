{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Y2019.AOC14
    ( solution1
    , solution2
    )
where
  
import           Text.Parsec.ByteString     (Parser, parseFromFile)
import           Text.Parsec                (digit, many1, sepBy, string, (<|>), letter, newline)
import Util (number)

newtype Count = Count Int deriving (Eq, Show, Num)

newtype Chemical = 
    Chemical {
        chemicalName :: String
    } deriving (Show, Eq)
  
data Reaction = Reaction {
    input :: [(Count, Chemical)],
    output :: (Count, Chemical)
} deriving (Show)

reactionParser :: Parser Reaction
reactionParser = do 
    input' <- sepBy ((,) <$> countParser <* string " " <*> chemicalParser) (string ", ")
    _ <- string " => "
    output' <- (,) <$> countParser <* string " " <*> chemicalParser
    return $ Reaction input' output'
    where countParser :: Parser Count
          countParser = Count <$> number
          chemicalParser :: Parser Chemical
          chemicalParser = Chemical <$> many1 letter

solution1 :: IO ()
solution1 = do
  ops <- parseFromFile (reactionParser `sepBy` newline) "data/example.input"
  print ops

solution2 :: IO ()
solution2 = error "no solution yet"

findReaction :: [Reaction] -> Chemical -> Reaction
findReaction [] _ = error "no reaction found"
findReaction (r:rs) c | snd (output r) == c = r
                      | otherwise = findReaction rs c

findCost :: [Reaction] -> Chemical -> ([(Count, Chemical)], Count)
findCost ops chem = (input $ findReaction ops chem, fst . output $ findReaction ops chem)

findOreCost :: [Reaction] -> Chemical -> Count
findOreCost ops chem =
    let cost = findCost ops chem
        oreCost = countOreAndRecurse cost
        countOreAndRecurse :: ([(Count, Chemical)], Count) -> Count
        countOreAndRecurse ([], _) = Count 0
        countOreAndRecurse ((c, Chemical "ORE"):cs, unit) = c + countOreAndRecurse (cs, unit)
        countOreAndRecurse ((c, chem'):cs, unit) = countOreAndRecurse (cs, unit) + (c * findOreCost ops chem')
    in oreCost

testData :: [Reaction]
testData = [
  Reaction {input = [(Count 10,Chemical {chemicalName = "ORE"})], output = (Count 10,Chemical {chemicalName = "A"})},
  Reaction {input = [(Count 1,Chemical {chemicalName = "ORE"})], output = (Count 1,Chemical {chemicalName = "B"})},
  Reaction {input = [(Count 7,Chemical {chemicalName = "A"}),(Count 1,Chemical {chemicalName = "B"})], output = (Count 1,Chemical {chemicalName = "C"})},
  Reaction {input = [(Count 7,Chemical {chemicalName = "A"}),(Count 1,Chemical {chemicalName = "C"})], output = (Count 1,Chemical {chemicalName = "D"})},
  Reaction {input = [(Count 7,Chemical {chemicalName = "A"}),(Count 1,Chemical {chemicalName = "D"})], output = (Count 1,Chemical {chemicalName = "E"})},
  Reaction {input = [(Count 7,Chemical {chemicalName = "A"}),(Count 1,Chemical {chemicalName = "E"})], output = (Count 1,Chemical {chemicalName = "FUEL"})}
  ]
