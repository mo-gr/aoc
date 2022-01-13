{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module AOC where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as C
import Data.Functor ((<&>))
import System.Directory
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual)
import Util (Input)

data Day
  = D1
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
day "1" = Just D1
day "2" = Just D2
day "3" = Just D3
day "4" = Just D4
day "5" = Just D5
day "6" = Just D6
day "7" = Just D7
day "8" = Just D8
day "9" = Just D9
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
day _ = Nothing

instance Enum Day where
  toEnum 1 = D1
  toEnum 2 = D2
  toEnum 3 = D3
  toEnum 4 = D4
  toEnum 5 = D5
  toEnum 6 = D6
  toEnum 7 = D7
  toEnum 8 = D8
  toEnum 9 = D9
  toEnum 10 = D10
  toEnum 11 = D11
  toEnum 12 = D12
  toEnum 13 = D13
  toEnum 14 = D14
  toEnum 15 = D15
  toEnum 16 = D16
  toEnum 17 = D17
  toEnum 18 = D18
  toEnum 19 = D19
  toEnum 20 = D20
  toEnum 21 = D21
  toEnum 22 = D22
  toEnum 23 = D23
  toEnum 24 = D24
  toEnum 25 = D25
  toEnum _ = error "no AOC day"
  fromEnum D1 = 1
  fromEnum D2 = 2
  fromEnum D3 = 3
  fromEnum D4 = 4
  fromEnum D5 = 5
  fromEnum D6 = 6
  fromEnum D7 = 7
  fromEnum D8 = 8
  fromEnum D9 = 9
  fromEnum D10 = 10
  fromEnum D11 = 11
  fromEnum D12 = 12
  fromEnum D13 = 13
  fromEnum D14 = 14
  fromEnum D15 = 15
  fromEnum D16 = 16
  fromEnum D17 = 17
  fromEnum D18 = 18
  fromEnum D19 = 19
  fromEnum D20 = 20
  fromEnum D21 = 21
  fromEnum D22 = 22
  fromEnum D23 = 23
  fromEnum D24 = 24
  fromEnum D25 = 25

data Year = Year
  { solution :: Day -> (IO String, IO String),
    inputDir :: String,
    showYear :: String,
    verify :: Test
  }

mkYear :: String -> (Day -> (IO String, IO String)) -> Test -> Year
mkYear y s v =
  Year
    { solution = s,
      inputDir = "/" ++ y,
      showYear = y,
      verify = v
    }

mkYear' :: String -> (Day -> Solution) -> Year
mkYear' y lookUpDay = mkYear y (runSolution =<< lookUpDay) (TestList $ mkTests <$> [D1 .. D25])
  where
    mkTests d =
      let input :: IO Input
          input = loadInput d
       in TestLabel ("Day " <> show (fromEnum d)) $
            TestList $ case lookUpDay d of
              PureSolution {part1, solution1, part2, solution2} ->
                [ TestCase $ assertEqual "solution 1" solution1 . part1 =<< input,
                  TestCase $ assertEqual "solution 2" solution2 . part2 =<< input
                ]
              IOSolution {part1IO, solution1, part2IO, solution2} ->
                [ TestCase $ input >>= part1IO >>= assertEqual "solution 1" solution1,
                  TestCase $ input >>= part2IO >>= assertEqual "solution 2" solution2
                ]

inputName :: Day -> String
inputName d = "AOC" <> show (fromEnum d)

data Solution where
  PureSolution ::
    (Show a, Eq a) =>
    { part1 :: Input -> a,
      solution1 :: a,
      part2 :: Input -> a,
      solution2 :: a
    } ->
    Solution
  IOSolution ::
    (Show a, Eq a) =>
    { part1IO :: Input -> IO a,
      solution1 :: a,
      part2IO :: Input -> IO a,
      solution2 :: a
    } ->
    Solution

runSolution :: Solution -> Day -> (IO String, IO String)
runSolution PureSolution {part1, part2} d = (loadInput d <&> part1 <&> show, loadInput d <&> part2 <&> show)
runSolution IOSolution {part1IO, part2IO} d = (loadInput d >>= part1IO <&> show, loadInput d >>= part2IO <&> show)

loadInput :: Day -> IO Input
loadInput d = C.readFile $ ("AOC" <> show (fromEnum d)) ++ ".input"

setupPath :: String -> IO FilePath
setupPath dir = do
  oldDirectory <- getCurrentDirectory
  setCurrentDirectory $ "data/" ++ dir
  pure oldDirectory

withPath :: String -> IO b -> IO b
withPath dir op =
  bracket
    (setupPath dir)
    setCurrentDirectory
    (const op)
