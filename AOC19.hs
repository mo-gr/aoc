module AOC19 where

import qualified Data.Map.Strict as M
import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec (string, digit, many1, char, endOfLine, oneOf, sepBy1, try, choice, runParser, eof, ParseError, parserTrace)
import Data.ByteString.Char8 (pack)

number :: Parser Int
number = read <$> many1 digit

leafParser :: Parser Rule
leafParser = do
  rid <- number <* string ": \""
  c <- oneOf "ab" <* string "\"" <* endOfLine
  return $ case c of
    'a' -> A rid
    'b' -> B rid
    _ -> error "unexpected rule"

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
  _<-endOfLine
  messages <- many1 messageParser
  return (rules, messages)

data Rule = A RuleName | B RuleName | Rule RuleName [RuleName] | Choice Int [RuleName] [RuleName] | Rule00
 deriving(Show)

ruleName :: Rule -> RuleName
ruleName (A rn) = rn
ruleName (B rn) = rn
ruleName (Rule rn _rs) = rn
ruleName (Choice rn _rs _rs') = rn
ruleName Rule00 = 0

parser :: Rules -> Rule -> Parser Message
parser _rules (A _) = (: []) <$> try (char 'a')
parser _rules (B _) = (: []) <$> try (char 'b')
parser rules (Rule _ rs) = let rs' = try <$> (parser rules . (M.!) rules <$> rs) in try $ mconcat rs'
parser rules (Choice 8 [42] [42, 8]) = let r' = try $ parser rules $ (M.!) rules 42 in try $ choice [
              try r',
              try (r' *> r'),
              try (r' *> r' *> r'),
              try (r' *> r' *> r' *> r'),
              try (r' *> r' *> r' *> r' *> r'),
              try (r' *> r' *> r' *> r' *> r' *> r'),
              try (r' *> r' *> r' *> r' *> r' *> r' *> r')
              ]
parser rules (Choice 11 [42, 31] [42, 11, 31]) = let r' = try $ parser rules $ (M.!) rules 42
                                                     r'' = try $ parser rules $ (M.!) rules 31
                                                 in try $ choice [
              try (r' *> r''),
              try (r' *> r' *> r'' *> r''),
              try (r' *> r' *> r' *> r'' *> r'' *> r''),
              try (r' *> r' *> r' *> r' *> r'' *> r'' *> r'' *> r''),
              try (r' *> r' *> r' *> r' *> r' *> r'' *> r'' *> r'' *> r'' *> r''),
              try (r' *> r' *> r' *> r' *> r' *> r' *> r'' *> r'' *> r'' *> r'' *> r'' *> r'')
              ]
parser rules Rule00 = let
  r42 = try $ parser rules $ (M.!) rules 42
  r31 = try $ parser rules $ (M.!) rules 31
  in try $ choice [
    r42 *> r42 *> r31,
    r42 *> r42 *> r42 *> r31 *> r31,
    r42 *> r42 *> r42 *> r42 *> r31 *> r31 *> r31,
    r42 *> r42 *> r42 *> r42 *> r42 *> r31 *> r31 *> r31 *> r31,
    r42 *> r42 *> r42 *> r31,
    r42 *> r42 *> r42 *> r42 *> r31 *> r31,
    r42 *> r42 *> r42 *> r42 *> r42 *> r31 *> r31 *> r31,
    r42 *> r42 *> r42 *> r42 *> r42 *> r42 *> r31 *> r31 *> r31 *> r31
  ]   
parser rules (Choice _ rs ls) = let rs' = try <$> (parser rules . (M.!) rules <$> rs)
                                    ls' = try <$> (parser rules . (M.!) rules <$> ls) in try $ choice [try (mconcat rs'), try (mconcat ls')]

type RuleName = Int
type Rules = M.Map RuleName Rule
type Message = String

indexRules :: [Rule] -> M.Map RuleName Rule
indexRules rs = M.fromList $ (\r -> (ruleName r, r)) <$> rs

isValid :: Rules -> Message -> Bool
isValid rules m = case parseMessage rules m of
                    Right _ -> True
                    Left _ -> False

parseMessage :: Rules -> Message -> Either ParseError ()
parseMessage rules m = let rule0 :: Rule
                           rule0 = (M.!) rules 0
                       in runParser (parser rules rule0 *> eof) () m (pack m)

-- 142
solution1 :: IO Int
solution1 = do
  Right (bareRules,messages) <- parseFromFile inputParser "AOC19.input"
  let rules :: Rules
      rules = indexRules bareRules
  return . length . filter id $ isValid rules <$> messages


--8: 42 | 42 8
--11: 42 31 | 42 11 31
rule8 :: Rule
rule8 = Choice 8 [42] [42, 8]

rule11 :: Rule
rule11 = Choice 11 [42, 31] [42, 11, 31]

--0: 8 11
rule00 :: Rule
rule00 = Rule00

-- not 178
solution2 :: IO Int
solution2 = do
  Right (bareRules,messages) <- parseFromFile inputParser "example.input"
  let rules :: Rules
      rules = indexRules bareRules
--      rules' = M.insert 0 rule00 rules
      rules' = M.insert 11 rule11 $ M.insert 8 rule8 rules
--  print "start: bbabbbbaabaabba"
  print $ parseMessage rules' "babbbbaabbbbbabbbbbbaabaaabaaa"
  return . length . filter id $ isValid rules' <$> messages
--  return 0
