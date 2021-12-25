{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC24 where

import Data.Functor (($>), (<&>))
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (char, many, many1, newline, space, string, (<|>), try)
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>), negativeNumber)
import Data.Maybe (fromJust)

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
mkAlu inp = Alu 0 0 0 0 inp

run :: Alu -> [AluOp] -> Alu
run a op = foldl (flip eval) a op

validModelNumber :: Alu -> Bool
validModelNumber a = getRegister Z a == 0

toNumber :: [Int] -> Int
toNumber [] = 0
toNumber (x:xs) = x * length xs + toNumber xs

validateModelNumber :: [AluOp] -> [Int] -> Maybe Int
validateModelNumber ops mn = run (mkAlu mn) ops |> \fin -> if validModelNumber fin then Just . toNumber $ mn else Nothing

prevModelNumber :: [Int] -> [Int]
prevModelNumber mn = case reverse mn of
  (1:rst) -> reverse $ 9 : reverse (prevModelNumber (reverse rst))
  (m:rst) -> reverse $ m - 1 : rst
  _ -> error "something went wrong"

firstJust :: [Maybe a] -> a
firstJust ((Just a):_) = a
firstJust (Nothing:rst) = firstJust rst
firstJust _ = error "no justice in this world"

findBiggest :: [AluOp] -> Int
findBiggest ops = firstJust $ validateModelNumber ops <$> mns
  where mns :: [[Int]]
        mns = iterate prevModelNumber $ replicate 14 9

solution1 :: Input -> Int
solution1 inp =
  parseOrDie inputParser inp
    |> findBiggest

solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> error "not yet"

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" undefined . solution1 =<< input,
      TestCase $ assertEqual "solution 2" undefined . solution2 =<< input
    ]

testData :: Input
testData = "inp z\ninp x\nmul z 3\neql z x\n"