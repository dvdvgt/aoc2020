{-|
Module      : AOC.Challenges.Day06
Description : Advent Of Code 2020 solutions for day 1 (https://adventofcode.com/2020/day/6)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day06 where

import AOC.Solution ( (:~>)(..) )

import qualified Data.Set as S
import Data.List.Split ( splitOn )
import Data.List ( foldl1' )

day06a :: [[String]] :~> Int
day06a = Solution
    { parse = map (splitOn "\n") . splitOn "\n\n"
    , solve = sum . map (S.size . S.fromList . concat)
    , output = show
    }

day06b :: [[String]] :~> Int
day06b = Solution
    { parse = map (splitOn "\n") . splitOn "\n\n"
    , solve = sum . map (S.size . foldl1' S.intersection . map S.fromList)
    , output = show
    }