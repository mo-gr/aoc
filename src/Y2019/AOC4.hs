module Y2019.AOC4 where


toDigits :: Int -> [Int]
toDigits x | x <= 9 = [x]
toDigits x = toDigits (div x 10) ++ [mod x 10]

inputRange :: [Int]
inputRange = [165432 .. 707912]

isMonotonous :: Int -> Bool
isMonotonous x =
  let digits = toDigits x
      f []           = True
      f (x' : y : _) | x' > y = False
      f (_ : xs)     = f xs
  in  f digits


hasDoubles :: Int -> Bool
hasDoubles x =
  let digits = toDigits x
      f []           = False
      f (x' : y : _) | x' == y = True
      f (_ : xs)     = f xs
  in  f digits

hasProperDoubles :: Int -> Bool
hasProperDoubles x =
  let digits = toDigits x
      f []                = False
      f (x' : y : z : xs) | x == y && y == z = f (filter (/= x') xs)
      f (x' : y : _)      | x' == y = True
      f (_ : xs)          = f xs
  in  f digits

search :: [Int] -> [Int]
search = filter (\n -> isMonotonous n && hasDoubles n)

search2 :: [Int] -> [Int]
search2 = filter (\n -> isMonotonous n && hasProperDoubles n)

-- 1716
solution1 :: IO Int
solution1 = do
  let passwords = search inputRange
  --print passwords
  return $ length passwords

-- 1163
solution2 :: IO Int
solution2 = do
  let passwords = search2 inputRange
  --print passwords
  return $ length passwords
