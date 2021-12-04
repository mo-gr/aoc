module Y2021.AOC where

import AOC (Day (..), Year (), mkYear)
import qualified Data.ByteString.Char8 as C
import Test.HUnit (Test (TestLabel, TestList))
import Util (Input)
import qualified Util (withPath)
import qualified Y2021.AOC1
import qualified Y2021.AOC2
import qualified Y2021.AOC3
import qualified Y2021.AOC4

year :: Year
year = mkYear "2021" solution verify

solution :: Day -> (IO String, IO String)
solution D1 = run Y2021.AOC1.solution1 Y2021.AOC1.solution2 $ loadInput "AOC1"
solution D2 = run Y2021.AOC2.solution1 Y2021.AOC2.solution2 $ loadInput "AOC2"
solution D3 = run Y2021.AOC3.solution1 Y2021.AOC3.solution2 $ loadInput "AOC3"
solution D4 = run Y2021.AOC4.solution1 Y2021.AOC4.solution2 $ loadInput "AOC4"
solution _ = error "not yet"

loadInput :: String -> IO Input
loadInput d = C.readFile $ d ++ ".input"

run :: (Show b, Show c) => (a -> b) -> (a -> c) -> IO a -> (IO String, IO String)
run f g a = (show . f <$> a, show . g <$> a)

verify :: Test
verify =
  TestList
    [ TestLabel "Day 1" $ Y2021.AOC1.verify (loadInput "AOC1"),
      TestLabel "Day 2" $ Y2021.AOC2.verify (loadInput "AOC2"),
      TestLabel "Day 3" $ Y2021.AOC3.verify (loadInput "AOC3"),
      TestLabel "Day 4" $ Y2021.AOC4.verify (loadInput "AOC4")
    ]

inRepl1 :: Day -> IO String
inRepl1 d = Util.withPath "2021" $ fst $ solution d

inRepl2 :: Day -> IO String
inRepl2 d = Util.withPath "2021" $ snd $ solution d
