{-|
Module      : AOC.Challenges.Day05
Description : Advent Of Code 2020 solutions for day 1 (https://adventofcode.com/2020/day/5)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day05 where

import AOC.Solution ( (:~>)(..) )
import Data.List ( foldl', sort, foldl1' )
import Data.Maybe ( fromMaybe )

seatId :: String -> Int
seatId = foldl' binary 0
    where
        binary :: Int -> Char -> Int
        binary n c = case c of
            'B' -> 2*n+1
            'R' -> 2*n+1
            _   -> 2*n

{-| 
    Check for missing seat ID. If the next ID is the current + 2 then the missing
    seat is between these two seats.
-}
gap :: [Int] -> Maybe Int
gap [] = Nothing
gap (x:y:xs)
    | x + 2 == y = Just (x + 1)
    | otherwise = gap xs

day05a :: [String] :~> Int
day05a = Solution
    { parse = lines
    , solve = foldl1' max . map seatId
    , printer = show
    }

day05b :: [String] :~> Int
day05b = Solution
    { parse = lines
    , solve = fromMaybe (-1) . gap . sort . map seatId
    , printer = show
    }