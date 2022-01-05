module Y2015.AOC where

import AOC (Day (..), Year (), mkYear)
import qualified Data.ByteString.Char8 as C
import Test.HUnit (Test (TestLabel, TestList))
import Util (Input)
import qualified Y2015.AOC1
import qualified Y2015.AOC2
import qualified Y2015.AOC3
import qualified Y2015.AOC4
import qualified Y2015.AOC5
import qualified Y2015.AOC6
import qualified Y2015.AOC7

year :: Year
year = mkYear "2015" solution verify

solution :: Day -> (IO String, IO String)
solution D1 = run Y2015.AOC1.solution1 Y2015.AOC1.solution2 $ loadInput "AOC1"
solution D2 = run Y2015.AOC2.solution1 Y2015.AOC2.solution2 $ loadInput "AOC2"
solution D3 = run Y2015.AOC3.solution1 Y2015.AOC3.solution2 $ loadInput "AOC3"
solution D4 = run Y2015.AOC4.solution1 Y2015.AOC4.solution2 $ loadInput "AOC4"
solution D5 = run Y2015.AOC5.solution1 Y2015.AOC5.solution2 $ loadInput "AOC5"
solution D6 = run Y2015.AOC6.solution1 Y2015.AOC6.solution2 $ loadInput "AOC6"
solution D7 = run Y2015.AOC7.solution1 Y2015.AOC7.solution2 $ loadInput "AOC7"
solution _ = error "not yet"

verify :: Test
verify =
  TestList
    [ TestLabel "Day 1" $ Y2015.AOC1.verify (loadInput "AOC1"),
      TestLabel "Day 2" $ Y2015.AOC2.verify (loadInput "AOC2"),
      TestLabel "Day 3" $ Y2015.AOC3.verify (loadInput "AOC3"),
      TestLabel "Day 4" $ Y2015.AOC4.verify (loadInput "AOC4"),
      TestLabel "Day 5" $ Y2015.AOC5.verify (loadInput "AOC5"),
      TestLabel "Day 6" $ Y2015.AOC6.verify (loadInput "AOC6"),
      TestLabel "Day 7" $ Y2015.AOC7.verify (loadInput "AOC7")
    ]

loadInput :: String -> IO Input
loadInput d = C.readFile $ d ++ ".input"

run :: (Show b, Show c) => (a -> b) -> (a -> c) -> IO a -> (IO String, IO String)
run f g a = (show . f <$> a, show . g <$> a)
