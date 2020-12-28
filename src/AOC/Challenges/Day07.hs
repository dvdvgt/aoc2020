{-|
Module      : AOC.Challenges.Day01
Description : Advent Of Code 2020 solutions for day 7 (https://adventofcode.com/2020/day/7)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day07 where

import AOC.Solution ( (:~>)(..) )
import Text.Parsec
import Data.Either (rights)
import qualified Text.Parsec as P
import qualified Data.Map.Strict as M
import Data.List.Split ( splitOn )
import qualified Data.Set as S
import Data.Functor ((<&>))
import Data.Maybe ( fromMaybe )

type Parser = Parsec String ()

type Graph v e = M.Map v (M.Map v e)
type Bag = String
type Count = Int

parse' :: Parser a -> String -> Either ParseError a
parse' p = P.parse p ""

rule :: Parser (Bag, [(Bag, Count)])
rule = do
  b <- bag
  string " contain "
  bs <- (string "no other bags" >> return []) <|> (bags `sepBy1` string ", ")
  char '.'
  return (b, bs)

bag :: Parser Bag
bag = do
  d1 <- many1 letter
  char ' '
  d2 <- many1 letter
  string " bag"
  optional $ char 's'
  return $ d1 ++ ' ':d2

bags :: Parser (Bag, Count)
bags = do
  n <- read <$> many1 digit
  char ' '
  b <- bag
  return (b, n)

parseToMap :: [(Bag, [(Bag, Count)])] -> [(Bag, M.Map Bag Count)]
parseToMap xs = [(bag, M.fromList bags) | (bag, bags) <- xs]

flipGraph :: Ord v => Graph v e -> Graph v e
flipGraph mp = M.fromListWith M.union
    [(m, M.singleton n e) | (n, ms) <- M.toList mp, (m, e ) <- M.toList ms]

allDescendants :: Ord v => Graph v e -> M.Map v (S.Set v)
allDescendants gr = descendantMap
  where
    descendantMap = M.foldMapWithKey (\v _ -> S.insert v (M.findWithDefault S.empty v descendantMap)) <$> gr

usageCounts :: Ord v => Graph v Int -> M.Map v Int
usageCounts gr = usageMap
  where
    usageMap = (\neighbors -> sum [ n * (M.findWithDefault 0 v usageMap + 1) | (v, n) <- M.toList neighbors]) <$> gr

day07a :: M.Map Bag (M.Map Bag Count) :~> Int
day07a = Solution
    { parse = M.fromList . parseToMap . rights . map (parse' rule) . splitOn "\n"
    , solve = length . fromMaybe S.empty . M.lookup "shiny gold" . allDescendants . flipGraph
    , output = show
    }

day07b :: M.Map Bag (M.Map Bag Count) :~> Maybe Int
day07b = Solution
    { parse = M.fromList . parseToMap . rights . map (parse' rule) . splitOn "\n"
    , solve = M.lookup "shiny gold" . usageCounts
    , output = show
    }