{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC18 where

import AOC (Solution (PureSolution))
import Text.Parsec (char, many, many1, newline, (<|>))
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

data SnailNumber = Number Int | Pair SnailNumber SnailNumber
  deriving (Eq)

isPair :: SnailNumber -> Bool
isPair (Pair _n _m) = True
isPair _ = False

instance Show SnailNumber where
  show (Number n) = show n
  show (Pair n m) = "[" ++ show n ++ "," ++ show m ++ "]"

snailParser :: Parser SnailNumber
snailParser = do
  _ <- char '['
  l <- snailParser <|> Number <$> number
  _ <- char ','
  r <- snailParser <|> Number <$> number
  _ <- char ']'
  pure $ Pair l r

inputParser :: Parser [SnailNumber]
inputParser = many1 (snailParser <* many newline)

addSnail :: SnailNumber -> SnailNumber -> SnailNumber
addSnail n m = Pair n m |> reduceSnail

reduceSnail :: SnailNumber -> SnailNumber
reduceSnail = reduceSnail' 0

reduceSnail' :: Int -> SnailNumber -> SnailNumber
reduceSnail' 10000 n = error $ "too deep " ++ show n
reduceSnail' r n =
  let expl = explode n
      splt = split expl
   in if n /= expl
        then reduceSnail' (r + 1) expl
        else
          if n /= splt
            then reduceSnail' (r + 1) splt
            else n

explode :: SnailNumber -> SnailNumber
explode sn = explode' 0 sn |> fst

addLeft :: Int -> SnailNumber -> SnailNumber
addLeft x (Number n) = Number $ x + n
addLeft x (Pair n m) = Pair (addLeft x n) m

addRight :: Int -> SnailNumber -> SnailNumber
addRight x (Number n) = Number $ x + n
addRight x (Pair n m) = Pair n (addRight x m)

explode' :: Int -> SnailNumber -> (SnailNumber, Maybe (Int, Int))
explode' _ (Number n) = (Number n, Nothing)
explode' 4 (Pair (Number n) (Number m)) = (Number 0, Just (n, m))
explode' rec (Pair n m) =
  let (nExp, nSpill) = explode' (rec + 1) n
      (mExp, mSpill) = explode' (rec + 1) m
   in case nSpill of
        Just (l, r) -> case (nExp, m) of
          (Number n', Number m') ->
            if isPair n
              then (Pair (Number n') (Number $ m' + r), Just (l, 0))
              else (Pair (Number $ n' + l) (Number $ m' + r), Nothing)
          (Number n', p) ->
            if isPair n
              then (Pair (Number n') (addLeft r p), Just (l, 0))
              else (Pair (Number $ n' + l) (addLeft r p), Nothing)
          (p, Number m') -> (Pair p (Number $ m' + r), Just (l, 0))
          (p, q) -> (Pair p (addLeft r q), Just (l, 0))
        Nothing -> case mSpill of
          Just (l, r) -> case (nExp, mExp) of
            (Number n', Number m') ->
              if isPair m
                then (Pair (Number $ n' + l) (Number m'), Just (0, r))
                else (Pair (Number $ n' + l) (Number $ m' + r), Nothing)
            (p, Number m') ->
              if isPair m
                then (Pair (addRight l p) (Number m'), Just (0, r))
                else (Pair (addRight l p) (Number $ m' + r), Nothing)
            (Number n', p) -> (Pair (Number $ n' + l) p, Just (0, r))
            (p, q) -> (Pair (addRight l p) q, Just (0, r))
          Nothing -> (Pair nExp mExp, Nothing)

split :: SnailNumber -> SnailNumber
split n = case split' n of
  Left sn -> sn
  Right sn -> sn

split' :: SnailNumber -> Either SnailNumber SnailNumber
split' (Number n) | n < 10 = Right $ Number n
split' (Number n) = Left $ Pair (Number (n `div` 2)) (Number ((n `div` 2) + (n `mod` 2)))
split' (Pair n m) = case split' n of
  Left snSplit -> Left $ Pair snSplit m
  Right snSplit -> case split' m of
    Right smSplit -> Right $ Pair snSplit smSplit
    Left smSplit -> Left $ Pair snSplit smSplit

magnitude :: SnailNumber -> Int
magnitude (Number n) = n
magnitude (Pair n m) = (3 * magnitude n) + (2 * magnitude m)

sumSnail :: [SnailNumber] -> SnailNumber
sumSnail = foldl1 addSnail

-- 3524
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> sumSnail
    |> magnitude

allPairs :: Eq a => [a] -> [(a, a)]
allPairs xs = filter (uncurry (/=)) $ do
  x <- xs
  y <- xs
  pure (x, y)

-- 4656
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> allPairs
    |> fmap (reduceSnail . uncurry addSnail)
    |> fmap magnitude
    |> maximum

testData :: Input
testData = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n"

solution :: Solution
solution = PureSolution solution1 3524 solution2 4656
