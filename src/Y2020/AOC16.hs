{-# LANGUAGE NamedFieldPuns #-}

module Y2020.AOC16 where

import AOC (Solution (PureSolution))
import Data.Bifunctor (second)
import qualified Data.Map.Strict as M
import Text.Parsec
  ( char,
    endOfLine,
    letter,
    many1,
    sepBy1,
    space,
    string,
    (<|>),
  )
import Text.Parsec.ByteString (Parser)
import Util (Input, number, parseOrDie, (|>))

ruleParser :: Parser Rule
ruleParser = do
  fn' <- letter
  fn <- many1 (letter <|> space)
  _ <- string ": "
  l1 <- number <* char '-'
  h1 <- number <* string " or "
  l2 <- number <* char '-'
  h2 <- number
  return $ Rule {fieldName = fn' : fn, validRange = [l1 .. h1] ++ [l2 .. h2]}

ticketParser :: Parser Ticket
ticketParser = do
  fs <- number `sepBy1` char ','
  return $ Ticket {fieldValues = fs}

inputParser :: Parser InputState
inputParser = do
  rules <- many1 (ruleParser <* endOfLine)
  _ <- endOfLine
  myTicket <- string "your ticket:" *> endOfLine *> ticketParser
  _ <- endOfLine *> endOfLine
  otherTickets <- string "nearby tickets:" *> endOfLine *> many1 (ticketParser <* endOfLine)
  return (rules, myTicket, otherTickets)

data Rule = Rule
  { fieldName :: String,
    validRange :: [Int]
  }
  deriving (Show)

newtype Ticket = Ticket {fieldValues :: [Int]}
  deriving (Show)

type InputState = ([Rule], Ticket, [Ticket])

invalidFields :: InputState -> [Int]
invalidFields (rules, _myTicket, tickets) = filter (invalid rules) $ concat (fieldValues <$> tickets)
  where
    invalid :: [Rule] -> Int -> Bool
    invalid rs field = all (\r -> field `notElem` r) (validRange <$> rs)

discardInvalidTickets :: InputState -> [Ticket]
discardInvalidTickets (rules, _myTicket, tickets) = filter (valid rules) tickets
  where
    valid :: [Rule] -> Ticket -> Bool
    valid rs Ticket {fieldValues} =
      let ranges = validRange <$> rs
       in all (\field -> field `elem` concat ranges) fieldValues

fieldCandidates :: Rule -> [Ticket] -> [Int]
fieldCandidates Rule {validRange} tickets = filter (\idx -> all (\Ticket {fieldValues} -> (fieldValues !! idx) `elem` validRange) tickets) [0 .. length (fieldValues (head tickets)) - 1]

-- 22057
solution1 :: Input -> Int
solution1 input = parseOrDie inputParser input |> sum . invalidFields

-- 1093427331937
solution2 :: Input -> Int
solution2 input =
  let state@(rules, myTicket, _tickets) = parseOrDie inputParser input
      candidates = M.fromList $ (\r@Rule {fieldName} -> (fieldName, fieldCandidates r (myTicket : discardInvalidTickets state))) <$> rules
      resolved = resolve candidates M.empty
      Ticket {fieldValues = myFields} = myTicket
   in (myFields !! (resolved M.! "departure location"))
        * (myFields !! (resolved M.! "departure station"))
        * (myFields !! (resolved M.! "departure platform"))
        * (myFields !! (resolved M.! "departure track"))
        * (myFields !! (resolved M.! "departure date"))
        * (myFields !! (resolved M.! "departure time"))

solution :: Solution
solution = PureSolution solution1 22057 solution2 1093427331937

resolve :: M.Map String [Int] -> M.Map String Int -> M.Map String Int
resolve candidates known | M.empty == candidates = known
resolve candidates known = case M.assocs $ M.filter (\xs -> length xs == 1) candidates of
  [(field, [entry])] -> resolve (updateCandidates (M.delete field candidates) entry) (M.insert field entry known)
  _ -> error "something went wrong"
  where
    updateCandidates :: M.Map String [Int] -> Int -> M.Map String [Int]
    updateCandidates m x = M.fromList $ second (filter (/= x)) <$> M.toList m
