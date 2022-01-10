module Y2015.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2015.AOC1
import qualified Y2015.AOC10
import qualified Y2015.AOC11
import qualified Y2015.AOC12
import qualified Y2015.AOC13
import qualified Y2015.AOC14
import qualified Y2015.AOC2
import qualified Y2015.AOC3
import qualified Y2015.AOC4
import qualified Y2015.AOC5
import qualified Y2015.AOC6
import qualified Y2015.AOC7
import qualified Y2015.AOC8
import qualified Y2015.AOC9

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2015.AOC1.solution
lookUpDay D2 = Y2015.AOC2.solution
lookUpDay D3 = Y2015.AOC3.solution
lookUpDay D4 = Y2015.AOC4.solution
lookUpDay D5 = Y2015.AOC5.solution
lookUpDay D6 = Y2015.AOC6.solution
lookUpDay D7 = Y2015.AOC7.solution
lookUpDay D8 = Y2015.AOC8.solution
lookUpDay D9 = Y2015.AOC9.solution
lookUpDay D10 = Y2015.AOC10.solution
lookUpDay D11 = Y2015.AOC11.solution
lookUpDay D12 = Y2015.AOC12.solution
lookUpDay D13 = Y2015.AOC13.solution
lookUpDay D14 = Y2015.AOC14.solution
lookUpDay _ = error "not yet"

year :: Year
year = mkYear' "2015" lookUpDay
