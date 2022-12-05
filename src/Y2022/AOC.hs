module Y2022.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2022.AOC1
import qualified Y2022.AOC2
import qualified Y2022.AOC3
import qualified Y2022.AOC4
import qualified Y2022.AOC5

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2022.AOC1.solution
lookUpDay D2 = Y2022.AOC2.solution
lookUpDay D3 = Y2022.AOC3.solution
lookUpDay D4 = Y2022.AOC4.solution
lookUpDay D5 = Y2022.AOC5.solution
lookUpDay _ = error "not started yet"

year :: Year
year = mkYear' "2022" lookUpDay
