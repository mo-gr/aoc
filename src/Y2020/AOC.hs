module Y2020.AOC where

import AOC (Day (..), Year (), Solution (), mkYear')
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

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2020.AOC1.solution
--lookUpDay D2 = Y2020.AOC2.solution
--lookUpDay D3 = Y2020.AOC3.solution
--lookUpDay D4 = Y2020.AOC4.solution
--lookUpDay D5 = Y2020.AOC5.solution
--lookUpDay D6 = Y2020.AOC6.solution
--lookUpDay D7 = Y2020.AOC7.solution
--lookUpDay D8 = Y2020.AOC8.solution
--lookUpDay D9 = Y2020.AOC9.solution
--lookUpDay D10 = Y2020.AOC10.solution
--lookUpDay D11 = Y2020.AOC11.solution
--lookUpDay D12 = Y2020.AOC12.solution
--lookUpDay D13 = Y2020.AOC13.solution
--lookUpDay D14 = Y2020.AOC14.solution
--lookUpDay D15 = Y2020.AOC15.solution
--lookUpDay D16 = Y2020.AOC16.solution
--lookUpDay D17 = Y2020.AOC17.solution
--lookUpDay D18 = Y2020.AOC18.solution
--lookUpDay D19 = Y2020.AOC19.solution
--lookUpDay D20 = Y2020.AOC20.solution
--lookUpDay D21 = Y2020.AOC21.solution
--lookUpDay D22 = Y2020.AOC22.solution
--lookUpDay D23 = Y2020.AOC23.solution
--lookUpDay D24 = Y2020.AOC24.solution
--lookUpDay D25 = Y2020.AOC25.solution

year :: Year
year = mkYear' "2020" lookUpDay