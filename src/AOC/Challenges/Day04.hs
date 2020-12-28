{-|
Module      : AOC.Challenges.Day04
Description : Advent Of Code 2020 solutions for day 4 (https://adventofcode.com/2020/day/4)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day04
( day04a
, day04b
) where

import AOC.Solution ( (:~>)(..) )

import Data.List.Split
    ( dropDelims, dropFinalBlank, oneOf, split, splitOn )
import qualified Data.Map.Strict as M
import Data.Maybe ( isJust )
import Control.Monad ( guard )
import Data.Char ( isDigit, isAlpha, isHexDigit, isLower )
import Text.Read ( readMaybe )

type Passport = M.Map String String

parseToList :: String -> [[String]]
parseToList = map (split . dropFinalBlank . dropDelims $ oneOf "\n ") . splitOn "\n\n"

parseToMap :: [[String]] -> [Passport]
parseToMap passports = do
    passport <- passports
    [M.fromList $ parsePassport passport]
    where
        parsePassport :: [String] -> [(String, String)]
        parsePassport p = do
            entry <- p
            let [key, value] =  splitOn ":" entry
            [(key, value)]

validatePassport ::  [Passport] -> [Passport]
validatePassport = filter (\p -> all (`M.member` p) fields)
    where
        fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validatePassportStrict :: [Passport] -> [Passport]
validatePassportStrict = filter isValid
    where
        isInRange :: (Int, Int) -> Int -> Bool
        isInRange (pX1, pX2) x = pX1 <= x && x <= pX2

        -- a # followed by exactly six characters 0-9 or a-f.
        isValidHcl :: String -> Bool
        isValidHcl (x:xs) = 
            let isValidChar     = \c -> isHexDigit c || ( isAlpha c && isLower c)
                hasNumPrefix    = x == '#'
            in all isValidChar xs && length xs == 6 && hasNumPrefix

        isValidPid :: String -> Bool
        isValidPid pid = length pid == 9 && all isDigit pid

        ecls :: [String]
        ecls = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

        isValid :: Passport -> Bool
        isValid p = isJust $ do
            -- Check birth year
            guard . isInRange (1920, 2002) =<< readMaybe =<< M.lookup "byr" p
            -- Check issue year
            guard . isInRange (2010, 2020) =<< readMaybe =<< M.lookup "iyr" p
            -- Check expiration year
            guard . isInRange (2020, 2030) =<< readMaybe =<< M.lookup "eyr" p
            -- Check height
            (hgtStr, hgtUnit) <- span isDigit <$> M.lookup "hgt" p
            hgt <- readMaybe hgtStr
            guard $ case hgtUnit of
                    "cm" -> isInRange (150, 193) hgt
                    "in" -> isInRange (59, 76) hgt
                    _ -> False
            -- Check hair colour
            guard . isValidHcl =<< M.lookup "hcl" p
            -- Check eye colour
            guard . (`elem` ecls) =<< M.lookup "ecl" p
            -- Check passport ID
            guard . isValidPid =<< M.lookup "pid" p

day04a :: [Passport] :~> Int
day04a = Solution
    { parse = parseToMap . parseToList
    , solve = length . validatePassport
    , output = show
    }

day04b :: [Passport] :~> Int
day04b = Solution
    { parse = parseToMap . parseToList
    , solve = length . validatePassportStrict . validatePassport
    , output = show
    }