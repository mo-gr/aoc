{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Y2015.AOC14 where

import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.Parsec.ByteString (Parser, parseFromFile)
import Util (Input, parseOrDie, (|>), number)
import Apecs
import Apecs.System (cmapIf)
import Text.Parsec (many1, letter, string, newline)
import Control.Monad (forM_)

newtype Position = Position Int deriving Show
newtype Velocity = Velocity (Int, Int, Int) deriving Show
newtype Stamina = Stamina Int deriving Show
newtype Resting = Resting Int deriving Show
newtype Score = Score Int deriving Show

makeWorldAndComponents "World" [''Position, ''Velocity, ''Stamina, ''Resting, ''Score]

inputParser :: Parser [(Int, Int, Int)]
inputParser = many1 $ do
  _name <- many1 letter
  _ <- string " can fly "
  vMax <- number
  _ <- string " km/s for "
  stami <- number
  _ <- string " seconds, but then must rest for "
  resting <- number
  _ <- string " seconds." <* newline
  pure (vMax, stami, resting)

createWorld :: [(Int, Int, Int)] -> System World ()
createWorld rs = do
  forM_ rs $ \r@(_,stamina,_) -> newEntity (Position 0, Velocity r, Stamina stamina, Score 0)

tick :: System World ()
tick = do
  cmap (\(Position p, Velocity (v, _, _), Not :: Not Resting) -> Position $ p + v)
  cmap (\(Resting r) -> Resting (pred r))
  cmap (\(Stamina s) -> Stamina (pred s))
  cmapIf (\(Stamina s) -> s == 0) (\(Velocity (_, _, rest)) -> (Resting rest, Not :: Not Stamina))
  cmapIf (\(Resting r) -> r == 0) (\(Velocity (_, stam, _)) -> (Stamina stam, Not :: Not Resting))
  maxDist <- cfold (\mDist (Position p) -> max mDist p) 0
  cmapIf (\(Position p) -> p == maxDist) (\(Score s) -> Score $ s + 1)


simulate :: Int -> [(Int, Int, Int)] -> IO ()
simulate ticks rs = initWorld >>= runSystem (
  do
    createWorld rs
    forM_ [1..ticks] $ const tick
--    cmapM_ (\(Position p) -> liftIO . print $ p)
    maxDist <- cfold (\mDist (Position p) -> max mDist p) 0
    maxScore <- cfold (\mScore (Score s) -> max mScore s) 0
    liftIO . putStrLn $ "max distance: " <> show maxDist
    liftIO . putStrLn $ "max score: " <> show maxScore
  )

main :: IO ()
main = do
  rsOrErr <- parseFromFile inputParser "data/2015/AOC14.input"
  case rsOrErr of
    Left e -> error$ show e
    Right rs -> simulate 2503 rs

-- 2640
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> error "not yet"

-- 1102
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> error "not yet"

verify :: IO Input -> Test
verify input =
  TestList
    [ TestCase $ assertEqual "solution 1" undefined . solution1 =<< input,
      TestCase $ assertEqual "solution 2" undefined . solution2 =<< input
    ]

testData :: Input
testData = ""