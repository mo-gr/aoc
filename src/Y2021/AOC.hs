module Y2021.AOC where

import AOC (Day (..), Year (), mkYear, withPath)
import qualified Data.ByteString.Char8 as C
import Test.HUnit (Test (TestLabel, TestList))
import Util (Input)
import qualified Y2021.AOC1
import qualified Y2021.AOC2
import qualified Y2021.AOC3
import qualified Y2021.AOC4
import qualified Y2021.AOC5
import qualified Y2021.AOC6
import qualified Y2021.AOC7
import qualified Y2021.AOC8
import qualified Y2021.AOC9
import qualified Y2021.AOC10
import qualified Y2021.AOC11
import qualified Y2021.AOC12
import qualified Y2021.AOC13
import qualified Y2021.AOC14
import qualified Y2021.AOC15
import qualified Y2021.AOC16
import qualified Y2021.AOC17
import qualified Y2021.AOC18
import qualified Y2021.AOC19
import qualified Y2021.AOC20
import qualified Y2021.AOC21
import qualified Y2021.AOC22
import qualified Y2021.AOC23
import qualified Y2021.AOC24
import qualified Y2021.AOC25

year :: Year
year = mkYear "2021" solution verify

solution :: Day -> (IO String, IO String)
solution D1 = run Y2021.AOC1.solution1 Y2021.AOC1.solution2 $ loadInput "AOC1"
solution D2 = run Y2021.AOC2.solution1 Y2021.AOC2.solution2 $ loadInput "AOC2"
solution D3 = run Y2021.AOC3.solution1 Y2021.AOC3.solution2 $ loadInput "AOC3"
solution D4 = run Y2021.AOC4.solution1 Y2021.AOC4.solution2 $ loadInput "AOC4"
solution D5 = run Y2021.AOC5.solution1 Y2021.AOC5.solution2 $ loadInput "AOC5"
solution D6 = run Y2021.AOC6.solution1 Y2021.AOC6.solution2 $ loadInput "AOC6"
solution D7 = run Y2021.AOC7.solution1 Y2021.AOC7.solution2 $ loadInput "AOC7"
solution D8 = run Y2021.AOC8.solution1 Y2021.AOC8.solution2 $ loadInput "AOC8"
solution D9 = run Y2021.AOC9.solution1 Y2021.AOC9.solution2 $ loadInput "AOC9"
solution D10 = run Y2021.AOC10.solution1 Y2021.AOC10.solution2 $ loadInput "AOC10"
solution D11 = run Y2021.AOC11.solution1 Y2021.AOC11.solution2 $ loadInput "AOC11"
solution D12 = run Y2021.AOC12.solution1 Y2021.AOC12.solution2 $ loadInput "AOC12"
solution D13 = run Y2021.AOC13.solution1 Y2021.AOC13.solution2 $ loadInput "AOC13"
solution D14 = run Y2021.AOC14.solution1 Y2021.AOC14.solution2 $ loadInput "AOC14"
solution D15 = run Y2021.AOC15.solution1 Y2021.AOC15.solution2 $ loadInput "AOC15"
solution D16 = run Y2021.AOC16.solution1 Y2021.AOC16.solution2 $ loadInput "AOC16"
solution D17 = run Y2021.AOC17.solution1 Y2021.AOC17.solution2 $ loadInput "AOC17"
solution D18 = run Y2021.AOC18.solution1 Y2021.AOC18.solution2 $ loadInput "AOC18"
solution D19 = run Y2021.AOC19.solution1 Y2021.AOC19.solution2 $ loadInput "AOC19"
solution D20 = run Y2021.AOC20.solution1 Y2021.AOC20.solution2 $ loadInput "AOC20"
solution D21 = run Y2021.AOC21.solution1 Y2021.AOC21.solution2 $ loadInput "AOC21"
solution D22 = run Y2021.AOC22.solution1 Y2021.AOC22.solution2 $ loadInput "AOC22"
solution D23 = run Y2021.AOC23.solution1 Y2021.AOC23.solution2 $ loadInput "AOC23"
solution D24 = run Y2021.AOC24.solution1 Y2021.AOC24.solution2 $ loadInput "AOC24"
solution D25 = run Y2021.AOC25.solution1 Y2021.AOC25.solution2 $ loadInput "AOC25"

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
      TestLabel "Day 4" $ Y2021.AOC4.verify (loadInput "AOC4"),
      TestLabel "Day 5" $ Y2021.AOC5.verify (loadInput "AOC5"),
      TestLabel "Day 6" $ Y2021.AOC6.verify (loadInput "AOC6"),
      TestLabel "Day 7" $ Y2021.AOC7.verify (loadInput "AOC7"),
      TestLabel "Day 8" $ Y2021.AOC8.verify (loadInput "AOC8"),
      TestLabel "Day 9" $ Y2021.AOC9.verify (loadInput "AOC9"),
      TestLabel "Day 10" $ Y2021.AOC10.verify (loadInput "AOC10"),
      TestLabel "Day 11" $ Y2021.AOC11.verify (loadInput "AOC11"),
      TestLabel "Day 12" $ Y2021.AOC12.verify (loadInput "AOC12"),
      TestLabel "Day 13" $ Y2021.AOC13.verify (loadInput "AOC13"),
      TestLabel "Day 14" $ Y2021.AOC14.verify (loadInput "AOC14"),
      TestLabel "Day 15" $ Y2021.AOC15.verify (loadInput "AOC15"),
      TestLabel "Day 16" $ Y2021.AOC16.verify (loadInput "AOC16"),
      TestLabel "Day 17" $ Y2021.AOC17.verify (loadInput "AOC17"),
      TestLabel "Day 18" $ Y2021.AOC18.verify (loadInput "AOC18"),
      TestLabel "Day 19" $ Y2021.AOC19.verify (loadInput "AOC19"),
      TestLabel "Day 20" $ Y2021.AOC20.verify (loadInput "AOC20"),
      TestLabel "Day 21" $ Y2021.AOC21.verify (loadInput "AOC21"),
      TestLabel "Day 22" $ Y2021.AOC22.verify (loadInput "AOC22"),
      TestLabel "Day 23" $ Y2021.AOC23.verify (loadInput "AOC23"),
      TestLabel "Day 24" $ Y2021.AOC24.verify (loadInput "AOC24"),
      TestLabel "Day 25" $ Y2021.AOC25.verify (loadInput "AOC25")
    ]

inRepl1 :: Day -> IO String
inRepl1 d = withPath "2021" $ fst $ solution d

inRepl2 :: Day -> IO String
inRepl2 d = withPath "2021" $ snd $ solution d
