module Y2017.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2017.AOC1
import qualified Y2017.AOC2
import qualified Y2017.AOC3
import qualified Y2017.AOC4

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2017.AOC1.solution
lookUpDay D2 = Y2017.AOC2.solution
lookUpDay D3 = Y2017.AOC3.solution
lookUpDay D4 = Y2017.AOC4.solution
lookUpDay _ = error "nothing yet"

year :: Year
year = mkYear' "2017" lookUpDay