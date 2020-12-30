{-|
Module      : AOC.Challenges.Day09
Description : Advent Of Code 2020 solutions for day 9 (https://adventofcode.com/2020/day/9)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day09
( day09a
, day09b
) where

import AOC.Solution ( (:~>)(..) )
import AOC.Common ( (!!?) )

import Text.Read ( readMaybe )
import Control.Monad ( guard )
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

solve2 :: Maybe [Int] -> Maybe Int
solve2 inpt = do
    toFind <- solve1 inpt
    inpt' <- inpt
    xs <- findRange inpt' toFind
    return (minimum xs + maximum xs)
    where
        findRange :: [Int] -> Int -> Maybe [Int]
        findRange xs target = go 0 [] xs
            where
                go _ _ [] = Nothing
                go 0 [] (y:ys)
                    | y >= target = go 0 [] ys
                    | otherwise = go y [y] ys
                go sum yss@(y:ys) zss@(z:zs)
                    | sum + z == target = Just (z : yss)
                    | sum + z > target = go (sum - y) ys zss
                    | otherwise = go (sum + z) (yss ++ [z]) zs

day09b :: Maybe [Int] :~> Maybe Int
day09b = Solution
    { parse = traverse readMaybe . lines
    , solve = solve2
    , output = show
    }