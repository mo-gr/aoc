module AOC where

data Day = D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  | D10
  | D11
  | D12
  | D13
  | D14
  | D15
  | D16
  | D17
  | D18
  | D19
  | D20
  | D21
  | D22
  | D23
  | D24
  | D25

day :: String -> Maybe Day
day "1"  = Just D1
day "2"  = Just D2
day "3"  = Just D3
day "4"  = Just D4
day "5"  = Just D5
day "6"  = Just D6
day "7"  = Just D7
day "8"  = Just D8
day "9"  = Just D9
day "10" = Just D10
day "11" = Just D11
day "12" = Just D12
day "13" = Just D13
day "14" = Just D14
day "15" = Just D15
day "16" = Just D16
day "17" = Just D17
day "18" = Just D18
day "19" = Just D19
day "20" = Just D20
day "21" = Just D21
day "22" = Just D22
day "23" = Just D23
day "24" = Just D24
day "25" = Just D25
day _    = Nothing

class AOC a where
  solution :: a -> Day -> (IO String, IO String)
  inputDir :: a -> String
  showYear :: a -> String

unify :: (Show a, Show b) => (IO a, IO b) -> (IO String, IO String)
unify (a, b) = (show <$> a, show <$> b)
