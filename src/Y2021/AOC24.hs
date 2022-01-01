module Y2021.AOC24 where

import Data.Functor (($>), (<&>))
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (char, many, many1, newline, space, string, (<|>), try)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), negativeNumber)
import Control.Monad (guard)

data Op a b
  = Inp a
  | Add a b
  | Mul a b
  | Div a b
  | Mod a b
  | Eql a b
  deriving (Show, Eq)

data Register = W | X | Y | Z deriving (Show, Eq)

getRegister :: Register -> (Alu -> Int)
getRegister W = w
getRegister X = x
getRegister Y = y
getRegister Z = z

setRegister :: Register -> Int -> Alu -> Alu
setRegister W n a = a {w = n}
setRegister X n a = a {x = n}
setRegister Y n a = a {y = n}
setRegister Z n a = a {z = n}

type AluOp = Op Register (Either Register Int)

data Alu = Alu
  { w :: Int,
    x :: Int,
    y :: Int,
    z :: Int,
    input :: [Int]
  } deriving (Show, Eq)

register :: Parser Register
register =
  char 'w' $> W
    <|> char 'x' $> X
    <|> char 'y' $> Y
    <|> char 'z' $> Z

opParser :: Parser AluOp
opParser =
  ((string "inp " *> register) <&> Inp)
    <|> ( string "add " *> do
            r <- register <* space
            rn <- (register <&> Left) <|> (negativeNumber <&> Right)
            pure $ Add r rn
        )
    <|> ( try (string "mul ") *> do
            r <- register <* space
            rn <- (register <&> Left) <|> (negativeNumber <&> Right)
            pure $ Mul r rn
        )
    <|> ( string "div " *> do
            r <- register <* space
            rn <- (register <&> Left) <|> (negativeNumber <&> Right)
            pure $ Div r rn
        )
    <|> ( string "mod " *> do
            r <- register <* space
            rn <- (register <&> Left) <|> (negativeNumber <&> Right)
            pure $ Mod r rn
        )
    <|> ( string "eql " *> do
            r <- register <* space
            rn <- (register <&> Left) <|> (negativeNumber <&> Right)
            pure $ Eql r rn
        )

inputParser :: Parser [AluOp]
inputParser = many1 (opParser <* many newline)

eval :: AluOp -> Alu -> Alu
eval (Inp r) a = setRegister r (head . input $ a) a {input=tail $ input a}
eval (Add r (Left b)) a = setRegister r (getRegister r a + getRegister b a) a
eval (Add r (Right b)) a = setRegister r (getRegister r a + b) a
eval (Mul r (Left b)) a = setRegister r (getRegister r a * getRegister b a) a
eval (Mul r (Right b)) a = setRegister r (getRegister r a * b) a
eval (Div r (Left b)) a = setRegister r (getRegister r a `div` getRegister b a) a
eval (Div r (Right b)) a = setRegister r (getRegister r a `div` b) a
eval (Mod r (Left b)) a = setRegister r (getRegister r a `mod` getRegister b a) a
eval (Mod r (Right b)) a = setRegister r (getRegister r a `mod` b) a
eval (Eql r (Left b)) a = setRegister r (if getRegister r a == getRegister b a then 1 else 0) a
eval (Eql r (Right b)) a = setRegister r (if getRegister r a == b then 1 else 0) a

mkAlu :: [Int] -> Alu
mkAlu = Alu 0 0 0 0

run :: Alu -> [AluOp] -> Alu
run = foldl (flip eval)

validModelNumber :: Alu -> Bool
validModelNumber a = getRegister Z a == 0

toNumber :: [Int] -> Int
toNumber [] = 0
toNumber (d:ds) = d * (10 ^ length ds) + toNumber ds

validateModelNumber :: [AluOp] -> [Int] -> Maybe Int
validateModelNumber ops mn = 
  run (mkAlu mn) ops 
  |> \fin -> if validModelNumber fin then Just . toNumber $ mn else Nothing

solution1 :: Input -> Int
solution1 inp =
  parseOrDie inputParser inp
    |> \ops -> case validateModelNumber ops (head (validSerials (reverse [1..9]))) of
       Nothing -> error "something is wrong"
       Just vmn -> vmn

solution2 :: Input -> Int
solution2 inp =
  parseOrDie inputParser inp
      |> \ops -> case validateModelNumber ops (head (validSerials [1..9])) of
         Nothing -> error "something is wrong"
         Just vmn -> vmn

verify :: IO Input -> Test
verify inp =
  TestList
    [ TestCase $ assertEqual "solution 1" 99299513899971 . solution1 =<< inp,
      TestCase $ assertEqual "solution 2" 93185111127911 . solution2 =<< inp
    ]

-- explanations stolen from https://github.com/dphilipson/advent-of-code-2021/blob/master/src/days/day24.rs
-- not sure I would have managed to figure this out myself...
--DIV, CHECK, OFFSET
--1, 14, 1, push i[0] + 1
--1, 15, 7, push i[1] + 7
--1, 15, 13, push i[2] + 13
--26, -6, 10, pop i[3] == popped - 6
--1, 14, 0, push i[4]
--26, -4, 13, pop i[5] - 4
--1, 15, 11, push i[6] + 11
--1, 15, 6, push i[7] + 6
--1, 11, 1, push i[8] + 1
--26, 0, 7, pop i[9]
--26, 0, 11, pop i[10]
--26, -3, 14, pop i[11] == popped - 3
--26, -9, 4, pop i[12] == popped - 9
--26, -9, 10, pop i[13] == popped - 9
--
--i3  == i2 + 7 (i3 -6 == i2 + 13)
--i5  == i4 - 4 (i5 - 4 = i4)
--i9  == i8 + 1
--i10 == i7 + 6
--i11 == i6 + 8 (11 - 3)
--i12 == i1 - 2 (7 - 9)
--i13 == i0 - 8 (1 - 9)

validSerials :: [Int] -> [[Int]]
validSerials serialDigits = do
  i0 <- serialDigits
  i1 <- serialDigits
  i2 <- serialDigits
  let i3 = i2 + 7
  guard $ i3 <= 9 && i3 > 0
  i4 <- serialDigits
  let i5 = i4 - 4
  guard $ i5 <= 9 && i5 > 0
  i6 <- serialDigits
  i7 <- serialDigits
  i8 <- serialDigits
  let i9 = i8 + 1
  guard $ i9 <= 9 && i9 > 0
  let i10 = i7 + 6
  guard $ i10 <= 9 && i10 > 0
  let i11 = i6 + 8
  guard $ i11 <= 9 && i11 > 0
  let i12 = i1 - 2
  guard $ i12 <= 9 && i12 > 0
  let i13 = i0 - 8
  guard $ i13 <= 9 && i13 > 0
  pure [i0,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13]

