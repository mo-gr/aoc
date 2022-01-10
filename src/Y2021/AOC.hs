module Y2021.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2021.AOC1
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
import qualified Y2021.AOC2
import qualified Y2021.AOC20
import qualified Y2021.AOC21
import qualified Y2021.AOC22
import qualified Y2021.AOC23
import qualified Y2021.AOC24
import qualified Y2021.AOC25
import qualified Y2021.AOC3
import qualified Y2021.AOC4
import qualified Y2021.AOC5
import qualified Y2021.AOC6
import qualified Y2021.AOC7
import qualified Y2021.AOC8
import qualified Y2021.AOC9

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2021.AOC1.solution
lookUpDay D2 = Y2021.AOC2.solution
lookUpDay D3 = Y2021.AOC3.solution
lookUpDay D4 = Y2021.AOC4.solution
lookUpDay D5 = Y2021.AOC5.solution
lookUpDay D6 = Y2021.AOC6.solution
lookUpDay D7 = Y2021.AOC7.solution
lookUpDay D8 = Y2021.AOC8.solution
lookUpDay D9 = Y2021.AOC9.solution
lookUpDay D10 = Y2021.AOC10.solution
lookUpDay D11 = Y2021.AOC11.solution
lookUpDay D12 = Y2021.AOC12.solution
lookUpDay D13 = Y2021.AOC13.solution
lookUpDay D14 = Y2021.AOC14.solution
lookUpDay D15 = Y2021.AOC15.solution
lookUpDay D16 = Y2021.AOC16.solution
lookUpDay D17 = Y2021.AOC17.solution
lookUpDay D18 = Y2021.AOC18.solution
lookUpDay D19 = Y2021.AOC19.solution
lookUpDay D20 = Y2021.AOC20.solution
lookUpDay D21 = Y2021.AOC21.solution
lookUpDay D22 = Y2021.AOC22.solution
lookUpDay D23 = Y2021.AOC23.solution
lookUpDay D24 = Y2021.AOC24.solution
lookUpDay D25 = Y2021.AOC25.solution

year :: Year
year = mkYear' "2021" lookUpDay
