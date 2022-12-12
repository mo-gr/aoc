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
lookUpDay _ = error "not started yet"

year :: Year
year = mkYear' "2022" lookUpDay
