module Y2016.AOC where
  
import AOC (Day (..), Solution, Year (), mkYear')
import qualified Y2016.AOC1

lookUpDay :: Day -> Solution
lookUpDay D1 = Y2016.AOC1.solution
lookUpDay _ = error "not yet"

year :: Year
year = mkYear' "2016" lookUpDay

