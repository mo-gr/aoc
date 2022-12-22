module Y2022.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2022.AOC1
import qualified Y2022.AOC2
import qualified Y2022.AOC3
import qualified Y2022.AOC4
import qualified Y2022.AOC5
import qualified Y2022.AOC6
import qualified Y2022.AOC7
import qualified Y2022.AOC8
import qualified Y2022.AOC9
import qualified Y2022.AOC10
import qualified Y2022.AOC11
import qualified Y2022.AOC12
import qualified Y2022.AOC13
import qualified Y2022.AOC14
import qualified Y2022.AOC15
import qualified Y2022.AOC16
import qualified Y2022.AOC17
import qualified Y2022.AOC18
import qualified Y2022.AOC19
import qualified Y2022.AOC20
import qualified Y2022.AOC21

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2022.AOC1.solution
lookUpDay D2 = Y2022.AOC2.solution
lookUpDay D3 = Y2022.AOC3.solution
lookUpDay D4 = Y2022.AOC4.solution
lookUpDay D5 = Y2022.AOC5.solution
lookUpDay D6 = Y2022.AOC6.solution
lookUpDay D7 = Y2022.AOC7.solution
lookUpDay D8 = Y2022.AOC8.solution
lookUpDay D9 = Y2022.AOC9.solution
lookUpDay D10 = Y2022.AOC10.solution
lookUpDay D11 = Y2022.AOC11.solution
lookUpDay D12 = Y2022.AOC12.solution
lookUpDay D13 = Y2022.AOC13.solution
lookUpDay D14 = Y2022.AOC14.solution
lookUpDay D15 = Y2022.AOC15.solution
lookUpDay D16 = Y2022.AOC16.solution
lookUpDay D17 = Y2022.AOC17.solution
lookUpDay D18 = Y2022.AOC18.solution
lookUpDay D19 = Y2022.AOC19.solution
lookUpDay D20 = Y2022.AOC20.solution
lookUpDay D21 = Y2022.AOC21.solution
lookUpDay _ = error "not started yet"

year :: Year
year = mkYear' "2022" lookUpDay
