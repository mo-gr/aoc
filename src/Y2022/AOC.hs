module Y2022.AOC where

import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2022.AOC1

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2022.AOC1.solution
lookUpDay _ = error "not started yet"

year :: Year
year = mkYear' "2022" lookUpDay
