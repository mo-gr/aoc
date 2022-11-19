module Y2017.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2017.AOC1
import qualified Y2017.AOC2
import qualified Y2017.AOC3
import qualified Y2017.AOC4
import qualified Y2017.AOC5
import qualified Y2017.AOC6
import qualified Y2017.AOC7
import qualified Y2017.AOC8
import qualified Y2017.AOC9
import qualified Y2017.AOC10
import qualified Y2017.AOC11
import qualified Y2017.AOC12
import qualified Y2017.AOC13

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2017.AOC1.solution
lookUpDay D2 = Y2017.AOC2.solution
lookUpDay D3 = Y2017.AOC3.solution
lookUpDay D4 = Y2017.AOC4.solution
lookUpDay D5 = Y2017.AOC5.solution
lookUpDay D6 = Y2017.AOC6.solution
lookUpDay D7 = Y2017.AOC7.solution
lookUpDay D8 = Y2017.AOC8.solution
lookUpDay D9 = Y2017.AOC9.solution
lookUpDay D10 = Y2017.AOC10.solution
lookUpDay D11 = Y2017.AOC11.solution
lookUpDay D12 = Y2017.AOC12.solution
lookUpDay D13 = Y2017.AOC13.solution
lookUpDay _ = error "nothing yet"

year :: Year
year = mkYear' "2017" lookUpDay