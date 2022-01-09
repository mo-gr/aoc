{-# LANGUAGE OverloadedStrings #-}

module Y2021.AOC19 where

import AOC (Solution (PureSolution))
import Data.List (delete, find, intersectBy, union)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec (char, many, many1, newline, string)
import Text.Parsec.ByteString (Parser)
import Util (Input, negativeNumber, number, parseOrDie, (|>))

type Point = (Int, Int, Int)

type Vector = (Int, Int, Int)

type Beacon = Point

type Scanner = (Maybe Point, [Beacon])

beaconParser :: Parser Beacon
beaconParser = do
  x <- negativeNumber <* char ','
  y <- negativeNumber <* char ','
  z <- negativeNumber <* newline
  pure (x, y, z)

scannerParser :: Parser Scanner
scannerParser = do
  -- --- scanner 0 ---
  sNo <- string "--- scanner " *> number <* string " ---" <* newline
  bs <- many1 beaconParser <* many newline
  pure $ if sNo == 0 then (Just (0, 0, 0), bs) else (Nothing, bs)

inputParser :: Parser [Scanner]
inputParser = many1 scannerParser

rotateX :: Point -> Point
rotateX (x, y, z) = (x, - z, y)

rotateY :: Point -> Point
rotateY (x, y, z) = (- z, y, x)

rotateZ :: Point -> Point
rotateZ (x, y, z) = (y, - x, z)

translate :: Point -> Point -> Point
translate (x, y, z) (x', y', z') = (x + x', y + y', z + z')

addV :: Point -> Point -> Point
addV = translate

subV :: Point -> Point -> Point
subV a b = addV a $ negateV b

negateV :: Vector -> Vector
negateV (x, y, z) = (- x, - y, - z)

adjust :: Scanner -> Point -> Scanner
adjust (Just _origin, _) _ = error "adjusting already adjusted scanner"
adjust (Nothing, bs) t = (Just (0, 0, 0), fmap (translate (negateV t)) bs)

manhattan :: Point -> Point -> Int
manhattan (x, y, z) (x', y', z') =
  let dx = x - x'
      dy = y - y'
      dz = z - z'
   in abs dx + abs dy + abs dz

distance :: Point -> Point -> Point
distance (x, y, z) (x', y', z') =
  let dx = x - x'
      dy = y - y'
      dz = z - z'
   in (abs dx, abs dy, abs dz)

delta :: Point -> Point -> Point
delta (x, y, z) (x', y', z') =
  let dx = x - x'
      dy = y - y'
      dz = z - z'
   in (dx, dy, dz)

allPairs :: Eq a => [a] -> [(a, a)]
allPairs as = filter (uncurry (/=)) $ do
  a <- as
  a' <- as
  pure (a, a')

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

candidates :: Scanner -> [Scanner] -> [Scanner]
candidates s ss = filter (/= s) ss |> fmap findPairWiseManhattan |> filter over12 |> fmap fst
  where
    distances :: S.Set Int
    distances = pairWiseManhattan (snd s) |> fmap fst3 |> S.fromList
    findPairWiseManhattan :: Scanner -> (Scanner, S.Set Int)
    findPairWiseManhattan s' = (s', pairWiseManhattan (snd s) |> fmap fst3 |> S.fromList)
    over12 :: (Scanner, S.Set Int) -> Bool
    over12 (_, distances') = S.intersection distances distances' |> length |> (>= 12)

intersectVia :: Eq b => (a -> b) -> [a] -> [a] -> [a]
intersectVia f = intersectBy (\a a' -> f a == f a')

matches :: [Point -> Point] -> Scanner -> Scanner -> Scanner -> Maybe (Scanner, Scanner)
matches tForms orig preTransform scan =
  let oDist = pairWiseDeltas . snd $ orig
      sDist = pairWiseDeltas . snd $ scan
      common = intersectVia fst3 oDist sDist
   in if length common >= 12
        then Just (preTransform, locate oDist sDist common scan)
        else
          if null tForms
            then Nothing
            else matches (tail tForms) orig preTransform (fmap (fmap (head tForms)) scan)

locate :: [(Vector, Beacon, Beacon)] -> [(Vector, Beacon, Beacon)] -> [(Vector, Beacon, Beacon)] -> Scanner -> Scanner
locate _orig _scan [] _s = error "unable to locate"
locate orig scan ((d, _b, _b') : _cs) (_, bs) =
  let (_, ob, ob') = fromJust $ find ((== d) . fst3) orig
      (_, sb, sb') = fromJust $ find ((== d) . fst3) scan
   in if subV ob sb == subV ob' sb'
        then (Just (subV ob sb), bs)
        else error "wrong assumption"

adjustScanner :: Scanner -> Scanner
adjustScanner (Nothing, bs) = (Nothing, bs)
adjustScanner (Just p, bs) = (Just (0, 0, 0), addV p <$> bs)

transforms :: [Point -> Point]
transforms =
  [ id,
    rotateX,
    rotateX,
    rotateX,
    rotateY . rotateX,
    rotateX,
    rotateX,
    rotateX,
    rotateY . rotateX,
    rotateX,
    rotateX,
    rotateX,
    rotateY . rotateX,
    rotateX,
    rotateX,
    rotateX,
    rotateZ . rotateX,
    rotateX,
    rotateX,
    rotateX,
    rotateZ . rotateZ . rotateX,
    rotateX,
    rotateX,
    rotateX
  ]

pairWiseManhattan :: [Beacon] -> [(Int, Beacon, Beacon)]
pairWiseManhattan bs = allPairs bs |> fmap (\(b, b') -> (manhattan b b', b, b'))

pairWiseDistances :: [Beacon] -> S.Set Vector
pairWiseDistances bs = allPairs bs |> fmap (uncurry distance) |> S.fromList

pairWiseDeltas :: [Beacon] -> [(Vector, Beacon, Beacon)]
pairWiseDeltas bs = allPairs bs |> fmap (\(b, b') -> (delta b b', b, b'))

runSolution :: [Scanner] -> S.Set Beacon
runSolution s = recur (head s) (tail s)
  where
    recur :: Scanner -> [Scanner] -> S.Set Beacon
    recur unifiedScanner unlocated | null unlocated = unifiedScanner |> snd |> S.fromList
    recur unifiedScanner unlocated =
      candidates unifiedScanner unlocated
        |> \cs -> case firstJust $ (\c -> matches transforms unifiedScanner c c) <$> cs of
          Nothing -> error "no solution"
          Just (preTransform, scanner) -> trace ("resolved scanner " ++ show (length unlocated) ++ " left") $ recur (unifyScanner unifiedScanner scanner) (delete preTransform unlocated)

runSolution2 :: [Scanner] -> [Point]
runSolution2 s = recur (head s) (tail s) [(0, 0, 0)]
  where
    recur :: Scanner -> [Scanner] -> [Point] -> [Point]
    recur _unifiedScanner unlocated scanLocs | null unlocated = scanLocs
    recur unifiedScanner unlocated scanLocs =
      candidates unifiedScanner unlocated
        |> \cs -> case firstJust $ (\c -> matches transforms unifiedScanner c c) <$> cs of
          Nothing -> error "no solution"
          Just (preTransform, scanner) ->
            trace ("resolved scanner " ++ show (length unlocated) ++ " left") $
              recur (unifyScanner unifiedScanner scanner) (delete preTransform unlocated) (fromJust (fst scanner) : scanLocs)

firstJust :: [Maybe a] -> Maybe a
firstJust (Just a : _) = Just a
firstJust [] = Nothing
firstJust (Nothing : others) = firstJust others

unifyScanner :: Scanner -> Scanner -> Scanner
unifyScanner (Just (0, 0, 0), bs) other = (Just (0, 0, 0), bs `union` (adjustScanner other |> snd))
unifyScanner _ _ = error "unify unlocated scanner"

-- 362
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> runSolution
    |> length

--12204
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> runSolution2
    |> pairWiseManhattan
    |> fmap fst3
    |> maximum

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" 362 . solution1 =<< input,
      TestCase $ assertEqual "solution 2" 12204 . solution2 =<< input
    ]

testData :: Input
testData = "--- scanner 0 ---\n404,-588,-901\n528,-643,409\n-838,591,734\n390,-675,-793\n-537,-823,-458\n-485,-357,347\n-345,-311,381\n-661,-816,-575\n-876,649,763\n-618,-824,-621\n553,345,-567\n474,580,667\n-447,-329,318\n-584,868,-557\n544,-627,-890\n564,392,-477\n455,729,728\n-892,524,684\n-689,845,-530\n423,-701,434\n7,-33,-71\n630,319,-379\n443,580,662\n-789,900,-551\n459,-707,401\n\n--- scanner 1 ---\n686,422,578\n605,423,415\n515,917,-361\n-336,658,858\n95,138,22\n-476,619,847\n-340,-569,-846\n567,-361,727\n-460,603,-452\n669,-402,600\n729,430,532\n-500,-761,534\n-322,571,750\n-466,-666,-811\n-429,-592,574\n-355,545,-477\n703,-491,-529\n-328,-685,520\n413,935,-424\n-391,539,-444\n586,-435,557\n-364,-763,-893\n807,-499,-711\n755,-354,-619\n553,889,-390\n\n--- scanner 2 ---\n649,640,665\n682,-795,504\n-784,533,-524\n-644,584,-595\n-588,-843,648\n-30,6,44\n-674,560,763\n500,723,-460\n609,671,-379\n-555,-800,653\n-675,-892,-343\n697,-426,-610\n578,704,681\n493,664,-388\n-671,-858,530\n-667,343,800\n571,-461,-707\n-138,-166,112\n-889,563,-600\n646,-828,498\n640,759,510\n-630,509,768\n-681,-892,-333\n673,-379,-804\n-742,-814,-386\n577,-820,562\n\n--- scanner 3 ---\n-589,542,597\n605,-692,669\n-500,565,-823\n-660,373,557\n-458,-679,-417\n-488,449,543\n-626,468,-788\n338,-750,-386\n528,-832,-391\n562,-778,733\n-938,-730,414\n543,643,-506\n-524,371,-870\n407,773,750\n-104,29,83\n378,-903,-323\n-778,-728,485\n426,699,580\n-438,-605,-362\n-469,-447,-387\n509,732,623\n647,635,-688\n-868,-804,481\n614,-800,639\n595,780,-596\n\n--- scanner 4 ---\n727,592,562\n-293,-554,779\n441,611,-461\n-714,465,-776\n-743,427,-804\n-660,-479,-426\n832,-632,460\n927,-485,-438\n408,393,-506\n466,436,-512\n110,16,151\n-258,-428,682\n-393,719,612\n-211,-452,876\n808,-476,-593\n-575,615,604\n-485,667,467\n-680,325,-822\n-627,-443,-432\n872,-547,-609\n833,512,582\n807,604,487\n839,-516,451\n891,-625,532\n-652,-548,-490\n30,-46,-14\n"

solution :: Solution
solution = PureSolution solution1 solution2 verify
