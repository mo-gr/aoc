{-# LANGUAGE OverloadedStrings #-}
module Y2019.AOC6
  ( solution1
  , solution2
  )
where

import           Data.ByteString        (ByteString)
import           Data.List              (find)
import           Data.Maybe             (fromMaybe)
import           Data.Set               (fromList, intersection, notMember)
import qualified Hedgehog               as H
import           Text.Parsec            (alphaNum, many1, parse, skipMany,
                                         space, string)
import           Text.Parsec.ByteString (Parser, parseFromFile)

newtype Planet = Planet {name :: String} deriving (Eq, Show, Ord)
data Orbit = Orbit { center :: Planet, outer :: Planet } deriving (Eq, Show)
type WeightedOrbit = (Orbit, Int)

com :: Planet
com = Planet "COM"

planetParser :: Parser Planet
planetParser = do
  name' <- many1 alphaNum
  return $ case name' of
    "COM" -> com
    _     -> Planet name'

orbitParser :: Parser Orbit
orbitParser = do
  center' <- planetParser
  _       <- string ")"
  Orbit center' <$> planetParser

inputParser :: Parser [Orbit]
inputParser = many1 (orbitParser <* skipMany space)

parentOrbit :: [Orbit] -> Planet -> Maybe Orbit
parentOrbit _ p  | p == com = Nothing
parentOrbit os p = find (\o' -> outer o' == p) os

calculateDistanceToCom :: [Orbit] -> Orbit -> WeightedOrbit
calculateDistanceToCom _ o | center o == com = (o, 1)
calculateDistanceToCom os o                  = fromMaybe (o, 1) $ do
  let c = center o
  po <- parentOrbit os c
  let pw = snd $ calculateDistanceToCom os po
  return (o, 1 + pw)

allParents :: [Orbit] -> Planet -> [Planet]
allParents os' = allParents' os' os' where
  allParents' _ [] _ = []
  allParents' os (o : _) p | outer o == p =
    center o : allParents' os os (center o)
  allParents' os (_o : os'') p = allParents' os os'' p

orbitSum :: [WeightedOrbit] -> Int
orbitSum = sum . map snd

transferCount :: [WeightedOrbit] -> Planet -> Planet -> Int
transferCount os p1 p2 =
  let orbits         = fst <$> os
      p1Parents      = allParents orbits p1
      p2Parents      = allParents orbits p2
      commonParents  = intersection (fromList p1Parents) (fromList p2Parents)
      withoutCommons = filter (`notMember` commonParents)
  in  length (withoutCommons p1Parents) + length (withoutCommons p2Parents)

-- 194721
solution1 :: IO Int
solution1 = do
  orbs <- parseFromFile inputParser "AOC6.input"
  case orbs of
    Right orbs' -> return $ orbitSum $ map (calculateDistanceToCom orbs') orbs'
    Left  e     -> error (show e)

-- 316
solution2 :: IO Int
solution2 = do
  orbs <- parseFromFile inputParser "AOC6.input"
  let you   = Planet "YOU"
      santa = Planet "SAN"
  case orbs of
    Right orbs' -> return
      $ transferCount (map (calculateDistanceToCom orbs') orbs') you santa
    Left e -> error (show e)

-- TESTS

prop_parser :: H.Property
prop_parser =
  H.withTests 1 $ H.property $ case parse inputParser "test" "COM)A" of
    Right x -> x H.=== [Orbit com (Planet "A")]
    Left  e -> H.footnote (show e) >> H.failure

prop_example :: H.Property
prop_example =
  H.withTests 1 $ H.property $ case parse inputParser "example" example of
    Right x ->
      (snd <$> map (calculateDistanceToCom x) x)
        H.=== [1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 7]
    Left e -> H.footnote (show e) >> H.failure

prop_transfer :: H.Property
prop_transfer =
  let you   = Planet "YOU"
      santa = Planet "SAN"
  in  H.withTests 1
        $ H.property
        $ case parse inputParser "example_transfer" transferExample of
            Right x ->
              transferCount (map (calculateDistanceToCom x) x) you santa H.=== 4
            Left e -> H.footnote (show e) >> H.failure

prop_parents :: H.Property
prop_parents =
  let you = Planet "YOU"
  in  H.withTests 1
        $ H.property
        $ case parse inputParser "example_transfer" transferExample of
            Right x ->
              (   name
                <$> allParents (fst <$> map (calculateDistanceToCom x) x) you
                )
                H.=== ["K", "J", "E", "D", "C", "B", "COM"]
            Left e -> H.footnote (show e) >> H.failure

example :: ByteString
example = "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L"

transferExample :: ByteString
transferExample = "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L K)YOU I)SAN"

_tests :: IO Bool
_tests = H.checkParallel $ H.Group
  "AOC5"
  [ ("prop_parser"  , prop_parser)
  , ("prop_example" , prop_example)
  , ("prop_transfer", prop_transfer)
  , ("prop_parents" , prop_parents)
  ]

