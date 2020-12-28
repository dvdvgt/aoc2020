{-|
Module      : AOC.Challenges.Day09
Description : Advent Of Code 2020 solutions for day 9 (https://adventofcode.com/2020/day/9)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day09 where

import AOC.Solution ( (:~>)(..) )
import AOC.Common

import Text.Read
import Control.Monad
import qualified Data.Set as S

-- PART 1

solve1 :: Maybe [Int] -> Maybe Int
solve1 inpt = do
    inpt' <- inpt
    validate inpt'
    where
        isSum :: [Int] -> Int -> Bool
        isSum xs x =
            let s = S.fromList xs
            in any (\y -> S.member (x - y) s) xs
        step :: [Int] -> Maybe Int
        step xs = do
            x <- xs !!? 25
            guard (not $ isSum (take 25 xs) x)
            return x
        validate :: [Int] -> Maybe Int
        validate [] = Nothing
        validate l@(x:xs) = case step l of
            Just n -> Just n
            Nothing -> validate xs
            

day09a :: Maybe [Int] :~> Maybe Int
day09a = Solution
    { parse = traverse readMaybe . lines
    , solve = solve1
    , output = show
    }

-- PART 2