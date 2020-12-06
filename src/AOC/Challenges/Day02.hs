{-|
Module      : AOC.Challenges.Day02
Description : Advent Of Code 2020 solutions for day 2 (https://adventofcode.com/2020/day/2)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day02
( day02a
, day02b
) where

import AOC.Solution ( (:~>)(..))

import Data.List.Split ( splitOn )


data Policy = Policy
    { pX1       :: Int
    , pX2       :: Int
    , pChar      :: Char
    , password  :: String
    } deriving (Show)

-- | O(n)
parser :: String -> Policy
parser line =
    let [range,c:_,pass] = words line
        [rangeFrom, rangeTo] = read <$> splitOn "-" range
    in Policy rangeFrom rangeTo c pass
    
-- | Checks whether a char's occurences in the password are within range. O(1)
validate1 :: Policy -> Bool
validate1 Policy {..} =
    let count = length . filter (== pChar)
        occurences = count password
    in pX1 <= occurences && occurences <= pX2

-- | Checks whether the password has a certain char at index pX1 XOR index pX2. O(n)
validate2 :: Policy -> Bool
validate2 Policy {..} =
    let x = password !! (pX1 - 1)
        y = password !! (pX2 - 1)
    in (x == pChar) /= (y == pChar)

day02a :: [Policy] :~> Int
day02a = Solution
    { parse     = map parser . lines
    , solve     = length . filter validate1
    , output   = show
    }

day02b :: [Policy] :~> Int
day02b = Solution
    { parse     = map parser . lines
    , solve     = length . filter validate2
    , output   = show
    }