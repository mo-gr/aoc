module Y2019.AOC where

import AOC (Day (..), Year (), mkYear)
import Test.HUnit (Test (TestList))
import qualified Y2019.AOC1
import qualified Y2019.AOC10
import qualified Y2019.AOC11
import qualified Y2019.AOC12
import qualified Y2019.AOC13
import qualified Y2019.AOC14
import qualified Y2019.AOC15
import qualified Y2019.AOC16
import qualified Y2019.AOC2
import qualified Y2019.AOC3
import qualified Y2019.AOC4
import qualified Y2019.AOC5
import qualified Y2019.AOC6
import qualified Y2019.AOC7
import qualified Y2019.AOC8
import qualified Y2019.AOC9

year :: Year
year = mkYear "2019" solution verify2019

solution :: Day -> (IO String, IO String)
solution D1 = showBoth (Y2019.AOC1.solution1, Y2019.AOC1.solution2)
solution D2 = showBoth (Y2019.AOC2.solution1, Y2019.AOC2.solution2)
solution D3 = showBoth (Y2019.AOC3.solution1, Y2019.AOC3.solution2)
solution D4 = showBoth (Y2019.AOC4.solution1, Y2019.AOC4.solution2)
solution D5 = showBoth (Y2019.AOC5.solution1, Y2019.AOC5.solution2)
solution D6 = showBoth (Y2019.AOC6.solution1, Y2019.AOC6.solution2)
solution D7 = showBoth (Y2019.AOC7.solution1, Y2019.AOC7.solution2)
solution D8 = showBoth (Y2019.AOC8.solution1, Y2019.AOC8.solution2)
solution D9 = showBoth (Y2019.AOC9.solution1, Y2019.AOC9.solution2)
solution D10 = showBoth (Y2019.AOC10.solution1, Y2019.AOC10.solution2)
solution D11 = showBoth (Y2019.AOC11.solution1, Y2019.AOC11.solution2)
solution D12 = showBoth (Y2019.AOC12.solution1, Y2019.AOC12.solution2)
solution D13 = showBoth (Y2019.AOC13.solution1, Y2019.AOC13.solution2)
solution D14 = showBoth (Y2019.AOC14.solution1, Y2019.AOC14.solution2)
solution D15 = showBoth (Y2019.AOC15.solution1, Y2019.AOC15.solution2)
solution D16 = showBoth (Y2019.AOC16.solution1, Y2019.AOC16.solution2)
solution _ = error "not yet"

showBoth :: (Show a, Show b) => (IO a, IO b) -> (IO String, IO String)
showBoth (a, b) = (show <$> a, show <$> b)

verify2019 :: Test
verify2019 =
  TestList
    []
