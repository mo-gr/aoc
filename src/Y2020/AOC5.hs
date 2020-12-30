module Y2020.AOC5 where

import           Data.Either            (fromRight)
import           Data.List              ((\\))
import           Text.Parsec            (char, count, many1, space, (<|>))
import           Text.Parsec.ByteString (Parser, parseFromFile)


type Pass =  ([Partition], [Partition])

data Partition = Front | Back deriving (Show)

partitions :: [Partition]
partitions = [Front, Back]

partitionParser :: Parser Partition
partitionParser = do
  p <- char 'F' <|> char 'B' <|> char 'L' <|> char 'R'
  return $ case p of
    'F' -> Front
    'B' -> Back
    'L' -> Front
    'R' -> Back
    _   -> error "something went wrong"

boardingPassParser :: Parser Pass
boardingPassParser = do
  rows' <- count 7 partitionParser
  cols' <- count 3 partitionParser
  _ <- many1 space
  return (rows', cols')

inputParser :: Parser  [Pass]
inputParser = many1 boardingPassParser

rows :: [Int]
rows = [0..127]

cols :: [Int]
cols = [0..7]

seatId :: Int -> Int -> Int
seatId row col = row * 8 + col

partition :: Partition -> [Int] -> [Int]
partition Front rs = take (length rs `div` 2) rs
partition Back rs  = drop (length rs `div` 2) rs

findSeatId :: [Int] -> [Int] -> Pass -> Int
findSeatId (r:_) (c:_) ([], []) = seatId r c
findSeatId rss css (r : rs, cs) = findSeatId (partition r rss) css (rs, cs)
findSeatId rss css ([], c : cs) = findSeatId rss (partition c css)  ([], cs)
findSeatId _ _ _                = error "something went wrong"

allPasses :: [Pass]
allPasses = [([r1, r2, r3, r4, r5, r6, r7],[c1, c2, c3]) |
  r1 <- partitions,
  r2 <- partitions,
  r3 <- partitions,
  r4 <- partitions,
  r5 <- partitions,
  r6 <- partitions,
  r7 <- partitions,
  c1 <- partitions,
  c2 <- partitions,
  c3 <- partitions
  ]

neighboursArePresent :: [Int] -> Int -> Bool
neighboursArePresent xs x = x + 1 `elem` xs && x - 1 `elem` xs

-- 890
solution1 :: IO Int
solution1 =  do
  passes <- fromRight [] <$> parseFromFile inputParser "AOC5.input"
  return . maximum $ findSeatId rows cols <$> passes

-- 651
solution2 :: IO Int
solution2 = do
  passes <- fromRight [] <$> parseFromFile inputParser "AOC5.input"
  let givenIds = findSeatId rows cols <$> passes
      allIds = findSeatId rows cols <$> allPasses
      missingIds = allIds \\ givenIds
  return . head $ filter (neighboursArePresent givenIds) missingIds
