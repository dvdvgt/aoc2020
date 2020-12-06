{-|
Module      : AOC.Challenges.Day01
Description : Advent Of Code 2020 solutions for day 1 (https://adventofcode.com/2020/day/1)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day01
( day01a
, day01b
) where

import AOC.Solution ( (:~>)(..) )

import qualified Data.Vector as Vec


findEntries :: Int -> Int -> Vec.Vector Int -> [[Int]]
findEntries 0 _ _ = []
findEntries 1 goal xs
    | goal `Vec.elem` xs    = [[goal]]
    | otherwise             = []
findEntries n goal xs = do
    (x, ys) <- vecTails xs
    let goal' = goal - x
    (x:) <$> findEntries (n - 1) goal' ys


vecTails :: Vec.Vector a -> [(a, Vec.Vector a)]
vecTails vec 
    | Vec.null vec = []
    | otherwise =
        let xs = Vec.tail vec
            x = Vec.head vec
        in (x, xs) : vecTails xs

day01a :: [Int] :~> [Int]
day01a = Solution
    { parse = map read . lines
    , solve = map product . findEntries 2 2020 . Vec.fromList
    , output = show
    }

day01b :: [Int] :~> [Int]
day01b = Solution
    { parse = map read . lines
    , solve = map product . findEntries 3 2020 . Vec.fromList
    , output = show
    }
