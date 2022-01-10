module Y2020.AOC4 where

import AOC (Solution (PureSolution))
import qualified Data.Set as S
import Text.Parsec
  ( alphaNum,
    char,
    count,
    endOfLine,
    letter,
    many1,
    sepBy1,
    space,
    string,
    (<|>),
  )
import Text.Parsec.ByteString (Parser)
import Util (Input, parseOrDie, (|>))

fieldParser :: Parser (String, String)
fieldParser = do
  fieldName <- count 3 letter <* string ":"
  fieldValue <- many1 (alphaNum <|> char '#') <* space
  return (fieldName, fieldValue)

passportParser :: Parser [(String, String)]
passportParser = many1 fieldParser

inputParser :: Parser [[(String, String)]]
inputParser = passportParser `sepBy1` endOfLine

isValidPassport :: [(String, String)] -> Bool
isValidPassport fields =
  let requiredFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- "cid" optional
      availableFields = S.fromList $ fst <$> fields
   in requiredFields `S.isSubsetOf` availableFields

isValidPassport' :: [(String, String)] -> Bool
isValidPassport' fields =
  let requiredFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- "cid" optional
      availableFields = S.fromList $ fst <$> fields
   in requiredFields `S.isSubsetOf` availableFields && all isValidPassportData fields

isValidPassportData :: (String, String) -> Bool
isValidPassportData ("byr", year) = let y = read year :: Int in y >= 1920 && y <= 2002
isValidPassportData ("iyr", year) = let y = read year :: Int in y >= 2010 && y <= 2020
isValidPassportData ("eyr", year) = let y = read year :: Int in y >= 2020 && y <= 2030
isValidPassportData ("hgt", h : e : i : "cm") = let h' = read [h, e, i] :: Int in h' >= 150 && h' <= 193
isValidPassportData ("hgt", h : e : "in") = let h' = read [h, e] :: Int in h' >= 59 && h' <= 76
isValidPassportData ("hgt", _) = False
isValidPassportData ("hcl", '#' : color) = all (`elem` "0123456789abcdef") color && length color == 6
isValidPassportData ("hcl", _) = False
isValidPassportData ("ecl", "amb") = True
isValidPassportData ("ecl", "blu") = True
isValidPassportData ("ecl", "brn") = True
isValidPassportData ("ecl", "gry") = True
isValidPassportData ("ecl", "grn") = True
isValidPassportData ("ecl", "hzl") = True
isValidPassportData ("ecl", "oth") = True
isValidPassportData ("ecl", _) = False
isValidPassportData ("pid", pid) = all (`elem` "0123456789") pid && length pid == 9
isValidPassportData _ = True

-- 260
solution1 :: Input -> Int
solution1 input =
  parseOrDie inputParser input
    |> filter isValidPassport
    |> length

-- 153
solution2 :: Input -> Int
solution2 input =
  parseOrDie inputParser input
    |> filter isValidPassport'
    |> length

solution :: Solution
solution = PureSolution solution1 260 solution2 153
