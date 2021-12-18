{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC18 where

import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>), number)
import Text.Parsec (char, (<|>), many1, newline, many)
import Debug.Trace

data SnailNumber = Number Int | Pair SnailNumber SnailNumber
  deriving (Eq)

isPair :: SnailNumber -> Bool
isPair (Pair n m) = True
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
addSnail = Pair

reduceSnail :: SnailNumber -> SnailNumber
reduceSnail = reduceSnail' 0

reduceSnail' :: Int -> SnailNumber -> SnailNumber
reduceSnail' 10000 n = error $ "too deep " ++  show n
reduceSnail' r n =  let exp = explode n
                        spt = split exp
                    in if n /= exp
                       then reduceSnail' (r+1) exp
                       else if n /= spt
                            then reduceSnail' (r+1) spt
                            else n

explode :: SnailNumber -> SnailNumber
explode sn = explode' 0 sn |> fst

addLeft :: Int -> SnailNumber -> SnailNumber
addLeft x (Number n) = Number $ x + n
addLeft x (Pair n m) = Pair (addLeft x n) m

addRight :: Int -> SnailNumber -> SnailNumber
addRight x (Number n) = Number $ x + n
addRight x (Pair n m) = Pair n (addRight x m)

ltr :: SnailNumber -> [Int]
ltr (Number n) = [n]
ltr (Pair n m) = ltr n ++ ltr m

explode' :: Int -> SnailNumber -> (SnailNumber, Maybe (Int, Int))
explode' _ (Number n) = (Number n, Nothing)
explode' 4 (Pair (Number n) (Number m)) = (Number 0, Just (n, m))
explode' rec (Pair n m) =
  let (nExp, nSpill) = explode' (rec+1) n
      (mExp, mSpill) = explode' (rec+1) m
  in case nSpill of
    Just (l,r) -> case (nExp, m) of
                    (Number n', Number m') -> if isPair n
                                             then (Pair (Number n') (Number $ m' + r), Just (l,0))
                                             else (Pair (Number $ n' + l) (Number $ m' + r), Nothing)
                    (Number n', p) -> if isPair n
                                      then (Pair (Number n') (addLeft r p), Just (l,0))
                                      else (Pair (Number $ n' + l) (addLeft r p), Nothing)
                    (p, Number m') -> (Pair p (Number $ m' + r), Just (l, 0))
                    (p, q) -> (Pair p (addLeft r q), Just (l,0))
    Nothing -> case mSpill of
      Just (l,r) -> case (nExp, mExp) of
                    (Number n', Number m') -> if isPair m
                                             then (Pair (Number $ n' + l) (Number m'), Just (0, r))
                                             else (Pair (Number $ n' + l) (Number $ m' + r), Nothing)
                    (p, Number m') -> if isPair m
                                      then (Pair (addRight l p) (Number m') , Just (0,r))
                                      else (Pair (addRight l p) (Number $ m' + r), Nothing)
                    (Number n', p) -> (Pair (Number $ n' + l) p, Just (0, r))
                    (p, q) -> (Pair (addRight l p) q, Just (0,r))
      Nothing -> (Pair nExp mExp, Nothing)


split sn = case split' sn of
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

split2 :: SnailNumber -> SnailNumber
split2 (Pair n m) = let n' = split2 n in
                    if isPair n' && not (isPair n) then Pair n' m else Pair n' (split2 m)
split2 (Number n) | n < 10 = Number n
split2 (Number n) = Pair (Number (n `div` 2)) (Number ((n `div` 2) + (n `mod` 2)))

magnitude :: SnailNumber -> Int
magnitude (Number n) = n
magnitude (Pair n m) = (3 * magnitude n) + (2 * magnitude m)

sumSnail :: [SnailNumber] -> SnailNumber
sumSnail = foldl1 f
  where f n m = addSnail n m |> reduceSnail


-- 3524
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> sumSnail
    |> magnitude

allPairs :: Eq a => [a] -> [(a,a)]
allPairs xs = filter (\(x,y) -> x /= y) $ do
  x <- xs
  y <- xs
  pure (x,y)


-- 4656
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> allPairs
    |> fmap (reduceSnail . uncurry addSnail)
    |> fmap magnitude
    |> maximum

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 3524 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 4656 . solution2 =<< input
    ]

testData :: Input
testData = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n"

