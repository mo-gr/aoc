{-# LANGUAGE NamedFieldPuns #-}

module AOC20 where

import           Data.Functor           ((<&>))
import           Data.List              (find, nub, transpose)
import           Data.Maybe             (catMaybes)
import           Text.Parsec            (char, count, digit, endOfLine, many1,
                                         string, (<|>))
import           Text.Parsec.ByteString (Parser, parseFromFile)

readPixel :: Char -> Pixel
readPixel '.' = White
readPixel '#' = Black
readPixel _   = error "invalid pixel"

number :: Parser Int
number = read <$> many1 digit

imageParser :: Parser Image
imageParser = do
  _ <- string "Tile "
  iid <- number <* char ':' <* endOfLine
  ps <- concat <$> count 10 (count 10 (char '.' <|> char '#') <* endOfLine)
  return $ Image {imageId=iid, dim=10, pixels=readPixel <$> ps}

inputParser :: Parser [Image]
inputParser = many1 (imageParser <* endOfLine)

data Pixel = Black | White deriving (Eq)

instance Show Pixel where
  show Black = "#"
  show White = "."

data Image = Image {
  imageId :: Int,
  dim     ::  Int,
  pixels  :: [Pixel]
} deriving (Eq)

instance Show Image where
  show Image {imageId} = show imageId

topLine :: Image -> [Pixel]
topLine Image {pixels, dim} = take dim pixels

bottomLine :: Image -> [Pixel]
bottomLine Image {pixels, dim} = reverse $ take dim $ reverse pixels

leftLine :: Image -> [Pixel]
leftLine Image {pixels, dim} = head <$> split dim pixels

rightLine :: Image -> [Pixel]
rightLine Image {pixels, dim} = last <$> split dim pixels

rotateImage :: Image -> Image
rotateImage i = i {pixels=rotate (dim i) (pixels i)}

flipImage :: Image -> Image
flipImage i = i {pixels=flipList (dim i) (pixels i)}

flipImage' :: Image -> Image
flipImage' i = i {pixels=flipList' (dim i) (pixels i)}

rotate :: Int -> [a] -> [a]
rotate n xs = concat $ reverse <$> transpose (split n xs)

flipList :: Int -> [a] -> [a]
flipList n xs = concat $ reverse (split n xs)

flipList' :: Int -> [a] -> [a]
flipList' n xs = concat $ reverse <$> split n xs

split :: Int  -> [a] -> [[a]]
split _n [] = []
split n xs  = take n xs : split n (drop n xs)

imageVariants :: Image -> [Image]
imageVariants i = [
  i,
  flipImage i,
  flipImage' i,
  rotateImage i,
  flipImage $ rotateImage i,
  flipImage' $ rotateImage i,
  rotateImage $ rotateImage i,
  rotateImage $ rotateImage $ rotateImage i
  ]

data Direction = DRight | DLeft | DTop | DBottom

matchingSide :: Direction -> Image -> Image -> Bool
matchingSide DRight i i'  = rightLine i == leftLine i'
matchingSide DLeft i i'   = leftLine i == rightLine i'
matchingSide DTop i i'    = topLine i == bottomLine i'
matchingSide DBottom i i' = bottomLine i == topLine i'

findMatching ::  [Image] -> Direction -> Image -> [Image]
findMatching [] _d _i = []
findMatching (i':is) d i | imageId i == imageId i' = findMatching is d  i
findMatching (i':is) d i = findMatching is d i
                                 ++ filter (matchingSide d i) (imageVariants i')

findMatchingNeighbours :: [Image] -> Image -> [Image]
findMatchingNeighbours is i = nub . concat $ (\d -> findMatching is d i) <$> [DRight, DLeft, DTop, DBottom]

-- 84116744709593
solution1 :: IO Int
solution1 = do
  Right is <- parseFromFile inputParser "AOC20.input" --"example.input"
  let two_neighbours = fst <$> filter (\ (_, xs) -> length xs == 2) ((\ i -> (i, findMatchingNeighbours is i)) <$> is)
--  print two_neighbours
  return $ product (imageId <$> two_neighbours)

head' :: [a] -> Maybe a
head' []    = Nothing
head' (a:_) = Just a

findNeighbours :: [Image] -> Direction -> Image -> [Image]
findNeighbours ns d i = case head' $ findMatching ns d i of
 Nothing -> [i]
 Just n  -> i : findNeighbours ns d n

imageLine :: Image -> Int -> [Pixel]
imageLine Image{} 0 = []
imageLine Image{dim} n | n >= (dim - 1) = []
imageLine Image{pixels, dim} n = tail . take (dim - 1) . drop (n * dim) $ pixels

fuseImages :: [[Image]] -> [Pixel]
fuseImages = foldr (\ i -> (++) (concat ([0 .. 8] <&> \ line -> concat $ flip imageLine line <$> i))) []

pretty :: Int -> [Pixel] -> String
pretty n pxs = unlines $ concatMap show <$> split n pxs

countWaves :: [Pixel] -> Int
countWaves []         = 0
countWaves (Black:ps) = 1 + countWaves ps
countWaves (_:ps)     = countWaves ps

at :: [a] -> Int -> Maybe a
at [] _     = Nothing
at (a:_) 0  = Just a
at (_:as) n = at as (n-1)

isBlack :: [[Pixel]] -> Int -> Int -> Bool
isBlack ps x y = (at ps y >>= (`at` x) ) == Just Black

-- 01234567890123456789
--0                  #
--1#    ##    ##    ###
--2 #  #  #  #  #  #
matchesSeaMonster :: [[Pixel]] -> Int -> Int -> Maybe (Int, Int)
matchesSeaMonster ps x y = if isBlack ps (x+18) (y+0)
  && isBlack ps (x+0) (y+1)
  && isBlack ps (x+5) (y+1)
  && isBlack ps (x+6) (y+1)
  && isBlack ps (x+11) (y+1)
  && isBlack ps (x+12) (y+1)
  && isBlack ps (x+17) (y+1)
  && isBlack ps (x+18) (y+1)
  && isBlack ps (x+19) (y+1)
  && isBlack ps (x+1) (y+2)
  && isBlack ps (x+4) (y+2)
  && isBlack ps (x+7) (y+2)
  && isBlack ps (x+10) (y+2)
  && isBlack ps (x+13) (y+2)
  && isBlack ps (x+16) (y+2) then Just (x,y) else Nothing

monsterSize :: Int
monsterSize = 15

detectMonster' :: [[Pixel]] -> [(Int,Int)]
detectMonster' ps = catMaybes. concat $ [0..(length ps)] <&> (\x -> [0..(length ps)] <&> matchesSeaMonster ps x)

detectMonster :: Int -> Image -> [(Int,Int)]
detectMonster n img = nub . concat $ (imageVariants img <&> (\img' -> detectMonster' (split n $ pixels img')))

-- 1957
solution2 :: IO Int
solution2 = do
  Right is <- parseFromFile inputParser "AOC20.input" --"example.input"
  let fieldAndNeighbours = (\ i -> (i, findMatchingNeighbours is i)) <$> is
  let aCorner = case rotateImage . rotateImage . rotateImage . fst <$> find (\(_i, is') -> length is' == 2) fieldAndNeighbours of
                 Just x  -> x
                 Nothing -> error "no corner found"
--  this rotation is correct for the example
--  let Just aCorner = rotateImage . fst <$> find (\(i, is) -> length is == 2) fieldAndNeighbours
  let firstCol = findNeighbours is DBottom aCorner
  let assembled =  findNeighbours is DRight <$> firstCol
  let fused = Image 0 96 $ fuseImages assembled
  let monsterCount = length $ detectMonster 96 fused
--  putStrLn $ pretty 96 $ pixels fused
  return $ countWaves (pixels fused) - (monsterCount * monsterSize)
