module Y2021.AOC3 where

import Data.Functor (($>))
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (char, many1, newline, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

data Bit = One | Zero deriving (Eq)

instance Show Bit where
  show One = "1"
  show Zero = "0"

newtype Number = Number {bits :: [Bit]} deriving (Eq, Show)

bitNumberParser :: Parser Number
bitNumberParser = do
  parsedBits <- many1 $ char '1' $> One <|> char '0' $> Zero
  _ <- newline
  pure $ Number parsedBits

inputParser :: Parser [Number]
inputParser = many1 bitNumberParser

shift :: Number -> Number
shift (Number (_ : bs)) = Number bs
shift _ = error "can't shift empty number"

invert :: Bit -> Bit
invert Zero = One
invert One = Zero

hasBit :: Bit -> Int -> Number -> Bool
hasBit checkBit n (Number bs) = (bs !! n) == checkBit

toInt :: Number -> Int
toInt (Number []) = 0
toInt (Number (Zero : bs)) = 0 + toInt (Number bs)
toInt (Number (One : bs)) = (2 ^ length bs) + toInt (Number bs)

mostCommonBitInPosition :: Int -> [Number] -> Bit
mostCommonBitInPosition 0 nums = mostCommonFirstBit nums
mostCommonBitInPosition n nums = mostCommonBitInPosition (n -1) (shift <$> nums)

mostCommonFirstBit :: [Number] -> Bit
mostCommonFirstBit nums =
  foldl countBits (0, 0) nums
    |> \(z, o) -> if z > o then Zero else One
  where
    countBits :: (Int, Int) -> Number -> (Int, Int)
    countBits (z, o) (Number (b : _)) = if b == Zero then (z + 1, o) else (z, o + 1)
    countBits _ _ = error "something went wrong"

gammaRate :: [Number] -> Number
gammaRate nums =
  let bitSize = length . bits . head $ nums
   in Number $ flip mostCommonBitInPosition nums <$> [0 .. (bitSize - 1)]

epsilonRate :: [Number] -> Number
epsilonRate nums =
  let bitSize = length . bits . head $ nums
   in Number $ invert . flip mostCommonBitInPosition nums <$> [0 .. (bitSize - 1)]

powerConsumption :: [Number] -> Int
powerConsumption nums =
  let gamma = toInt $ gammaRate nums
      epsilon = toInt $ epsilonRate nums
   in gamma * epsilon

oxygenRating :: [Number] -> Int -> Number
oxygenRating [] _ = error "something gone wrong"
oxygenRating [n] _ = n
oxygenRating nums n =
  let filterBit = mostCommonBitInPosition n nums
   in oxygenRating (filter (hasBit filterBit n) nums) (n + 1)

co2Rating :: [Number] -> Int -> Number
co2Rating [] _ = error "something gone wrong"
co2Rating [n] _ = n
co2Rating nums n =
  let filterBit = invert $ mostCommonBitInPosition n nums
   in co2Rating (filter (hasBit filterBit n) nums) (n + 1)

lifeSupport :: [Number] -> Int
lifeSupport nums =
  let o2 = toInt $ oxygenRating nums 0
      co2 = toInt $ co2Rating nums 0
   in o2 * co2

-- 1092896
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> powerConsumption

-- 4672151
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> lifeSupport

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 1092896 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 4672151 . solution2 =<< input
    ]
