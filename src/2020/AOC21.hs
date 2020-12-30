module AOC21 where

import           Data.Either            (fromRight)
import           Data.Functor           ((<&>))
import           Data.List              (intercalate, nub, sortBy)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import           Text.Parsec            (endBy1, endOfLine, letter, many1,
                                         sepBy1, space, string, try)
import           Text.Parsec.ByteString (Parser, parseFromFile)

foodParser :: Parser Food
foodParser = do
  ings <- try $ many1 letter `endBy1` space
  _ <- string "(contains "
  alls <- many1 letter `sepBy1` string ", "
  _ <- string ")" <* endOfLine
  return (ings, alls)

inputParser :: Parser [Food]
inputParser = many1 foodParser

type Food = ([Ingredient],[Allergen])
type Ingredient = String
type Allergen = String

type Candidates = M.Map Allergen (S.Set Ingredient)

toCandidateSet :: [Food] -> Candidates -> Candidates
toCandidateSet [] c = c
toCandidateSet ((ingredients, allergen):fs) c = toCandidateSet fs (foldl f c allergen)
  where f :: Candidates -> Allergen -> Candidates
        f c' a = M.alter g a c'
        g :: Maybe (S.Set Ingredient) -> Maybe (S.Set Ingredient)
        g (Just s) = Just $ S.intersection s $ S.fromList ingredients
        g Nothing  = Just $ S.fromList ingredients

-- 1977
solution1 :: IO Int
solution1 = do
  food <- fromRight [] <$> parseFromFile inputParser "AOC21.input"
  let ingredients = nub . concat $ fst <$> food
  let candidates = toCandidateSet food M.empty
  let safeIngredients = S.fromList ingredients S.\\ M.foldr S.union S.empty candidates
  return . sum $ S.toList safeIngredients <&> (\i -> length . filter (== i) $ concat (fst <$> food) )

sanitize :: Candidates -> Candidates
sanitize c = M.fromList $ go <$> M.assocs c
  where go :: (Allergen, S.Set Ingredient) -> (Allergen, S.Set Ingredient)
        go (a, s) | length s == 1 = (a, s)
        go (a, orig_s) = (a, M.foldl (\s s' -> if length s' ==  1 then s S.\\ s' else s) orig_s c)

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f a = let a' = f a in if a == a' then a else untilStable f a'

pprint :: Candidates -> String
pprint cs = intercalate "," $ head . S.toList . snd <$> sortBy (\ (i, _a) (i', _a') -> compare i i') (M.toList cs)

-- dpkvsdk,xmmpt,cxjqxbt,drbq,zmzq,mnrjrf,kjgl,rkcpxs
solution2 :: IO String
solution2 = do
  food <- fromRight [] <$> parseFromFile inputParser  "AOC21.input"
  let candidates = toCandidateSet food M.empty
  let allergenes = untilStable sanitize candidates
  return $ pprint allergenes
