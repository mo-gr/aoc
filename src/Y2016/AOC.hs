module Y2016.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2016.AOC1
import qualified Y2016.AOC2
import qualified Y2016.AOC3
import qualified Y2016.AOC4

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2016.AOC1.solution
lookUpDay D2 = Y2016.AOC2.solution
lookUpDay D3 = Y2016.AOC3.solution
lookUpDay D4 = Y2016.AOC4.solution
lookUpDay _ = error "not yet"

year :: Year
year = mkYear' "2016" lookUpDay

