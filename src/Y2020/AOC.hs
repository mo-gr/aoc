module Y2020.AOC where

import AOC (Day (..), Year (), mkYear, showBoth)
import Test.HUnit (Test (TestList))
import qualified Y2020.AOC1
import qualified Y2020.AOC10
import qualified Y2020.AOC11
import qualified Y2020.AOC12
import qualified Y2020.AOC13
import qualified Y2020.AOC14
import qualified Y2020.AOC15
import qualified Y2020.AOC16
import qualified Y2020.AOC17
import qualified Y2020.AOC18
import qualified Y2020.AOC19
import qualified Y2020.AOC2
import qualified Y2020.AOC20
import qualified Y2020.AOC21
import qualified Y2020.AOC22
import qualified Y2020.AOC23
import qualified Y2020.AOC24
import qualified Y2020.AOC25
import qualified Y2020.AOC3
import qualified Y2020.AOC4
import qualified Y2020.AOC5
import qualified Y2020.AOC6
import qualified Y2020.AOC7
import qualified Y2020.AOC8
import qualified Y2020.AOC9

year :: Year
year = mkYear "2020" solution verify2020

solution :: Day -> (IO String, IO String)
solution D1 = showBoth (Y2020.AOC1.solution1, Y2020.AOC1.solution2)
solution D2 = showBoth (Y2020.AOC2.solution1, Y2020.AOC2.solution2)
solution D3 = showBoth (Y2020.AOC3.solution1, Y2020.AOC3.solution2)
solution D4 = showBoth (Y2020.AOC4.solution1, Y2020.AOC4.solution2)
solution D5 = showBoth (Y2020.AOC5.solution1, Y2020.AOC5.solution2)
solution D6 = showBoth (Y2020.AOC6.solution1, Y2020.AOC6.solution2)
solution D7 = showBoth (Y2020.AOC7.solution1, Y2020.AOC7.solution2)
solution D8 = showBoth (Y2020.AOC8.solution1, Y2020.AOC8.solution2)
solution D9 = showBoth (Y2020.AOC9.solution1, Y2020.AOC9.solution2)
solution D10 = showBoth (Y2020.AOC10.solution1, Y2020.AOC10.solution2)
solution D11 = showBoth (Y2020.AOC11.solution1, Y2020.AOC11.solution2)
solution D12 = showBoth (Y2020.AOC12.solution1, Y2020.AOC12.solution2)
solution D13 = showBoth (Y2020.AOC13.solution1, Y2020.AOC13.solution2)
solution D14 = showBoth (Y2020.AOC14.solution1, Y2020.AOC14.solution2)
solution D15 = showBoth (Y2020.AOC15.solution1, Y2020.AOC15.solution2)
solution D16 = showBoth (Y2020.AOC16.solution1, Y2020.AOC16.solution2)
solution D17 = showBoth (Y2020.AOC17.solution1, Y2020.AOC17.solution2)
solution D18 = showBoth (Y2020.AOC18.solution1, Y2020.AOC18.solution2)
solution D19 = showBoth (Y2020.AOC19.solution1, Y2020.AOC19.solution2)
solution D20 = showBoth (Y2020.AOC20.solution1, Y2020.AOC20.solution2)
solution D21 = showBoth (Y2020.AOC21.solution1, Y2020.AOC21.solution2)
solution D22 = showBoth (Y2020.AOC22.solution1, Y2020.AOC22.solution2)
solution D23 = showBoth (Y2020.AOC23.solution1, Y2020.AOC23.solution2)
solution D24 = showBoth (Y2020.AOC24.solution1, Y2020.AOC24.solution2)
solution D25 = showBoth (Y2020.AOC25.solution1, Y2020.AOC25.solution2)

verify2020 :: Test
verify2020 =
  TestList
    []
