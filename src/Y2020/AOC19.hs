module Y2020.AOC19 where

import           Data.ByteString.Char8  (pack)
import           Data.List              (nub)
import qualified Data.Map.Strict        as M
import           Text.Parsec            (ParseError, char, choice, endOfLine,
                                         eof, many1, oneOf, runParser, sepBy1,
                                         string, try, (<|>))
import           Text.Parsec.ByteString (Parser, parseFromFile)
import           Util                   (number)

leafParser :: Parser Rule
leafParser = do
  rid <- number <* string ": \""
  c <- oneOf "ab" <* string "\"" <* endOfLine
  return $ case c of
    'a' -> A rid
    'b' -> B rid
    _   -> error "unexpected rule"

choiceParser :: Parser Rule
choiceParser = do
  rid <- number <* string ": "
  left <- many1 (number <* char ' ')
  _ <- string "| "
  right <- number `sepBy1` char ' '
  _ <- endOfLine
  return $ Choice rid left right

ruleParser :: Parser Rule
ruleParser = do
  rid <- number <* string ": "
  rules <- number `sepBy1` char ' '
  _ <- endOfLine
  return $ Rule rid rules

messageParser :: Parser Message
messageParser = many1 (oneOf "ab") <* endOfLine

inputParser :: Parser ([Rule], [Message])
inputParser = do
  rules <- many1 (choice [try ruleParser,try choiceParser,leafParser])-- <|> leafParser)
  _ <- endOfLine
  messages <- many1 messageParser
  return (rules, messages)

data Rule = A RuleName | B RuleName | Rule RuleName [RuleName] | Choice Int [RuleName] [RuleName]
 deriving(Show)

ruleName :: Rule -> RuleName
ruleName (A rn)               = rn
ruleName (B rn)               = rn
ruleName (Rule rn _rs)        = rn
ruleName (Choice rn _rs _rs') = rn

parser :: Rules -> Rule -> Parser Message
parser _rules (A _) = (: []) <$> char 'a'
parser _rules (B _) = (: []) <$> char 'b'
parser rules (Rule _ rs) = let rs' = (parser rules . (M.!) rules <$> rs) in mconcat rs'
parser rules (Choice _ rs ls) = let rs' = (parser rules . (M.!) rules <$> rs)
                                    ls' = (parser rules . (M.!) rules <$> ls) in try (mconcat rs') <|> mconcat ls'

type RuleName = Int
type Rules = M.Map RuleName Rule
type Message = String

indexRules :: [Rule] -> M.Map RuleName Rule
indexRules rs = M.fromList $ (\r -> (ruleName r, r)) <$> rs

isValid :: Rules -> Message -> Bool
isValid rules m = case parseMessage rules m of
                    Right _ -> True
                    Left _  -> False

parseMessage :: Rules -> Message -> Either ParseError ()
parseMessage rules m = let rule0 :: Rule
                           rule0 = (M.!) rules 0
                       in runParser (parser rules rule0 *> eof) () m (pack m)

-- 142
solution1 :: IO Int
solution1 = do
  Right (bareRules, messages) <- parseFromFile inputParser "AOC19.input"
  let rules :: Rules
      rules = indexRules bareRules
  return . length . filter id $ isValid rules <$> messages

-- let's unroll the loop at the start manually a few times :sadface:
rule0s :: [Rule]
rule0s = [
  Rule 0 [42, 42, 31],
  Rule 0 [42, 42, 42, 31] ,
  Rule 0 [42, 42, 42, 42, 31] ,
  Rule 0 [42, 42, 42, 42, 42, 31] ,
  Rule 0 [42, 42, 42, 42, 42, 42, 31] ,
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 31] ,
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 31],
  Rule 0 [42, 42, 42, 31],
  Rule 0 [42, 42, 42, 31, 31],
  Rule 0 [42, 42, 42, 42, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31],
  Rule 0 [42, 42, 42, 42, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 31, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31, 31],
  Rule 0 [42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 31, 31, 31, 31, 31]
  ]

-- 294
solution2 :: IO Int
solution2 = do
  Right (bareRules, messages) <- parseFromFile inputParser "AOC19.input"
  let rules :: Rules
      rules = indexRules bareRules
  return . length . nub $ [message | message<-messages, r0<-rule0s, isValid (M.insert 0 r0 rules) message]
