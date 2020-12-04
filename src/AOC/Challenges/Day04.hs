module AOC.Challenges.Day04
( day04a
, day04b
) where

import AOC.Solution ( (:~>)(..) )

import Data.List.Split
    ( dropDelims, dropFinalBlank, onSublist, oneOf, split, splitOn )
import qualified Data.Map.Strict as M
import Data.Maybe ( isJust )
import Control.Monad ( guard )
import Data.Char ( isDigit, isAlpha, isHexDigit, isLower )
import Text.Read ( readMaybe )

type Passport = M.Map String String

parseToList :: String -> [[String]]
parseToList = map (split . dropFinalBlank . dropDelims $ oneOf "\n ") . split (dropFinalBlank . dropDelims $ onSublist "\n\n")

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
            let isValidChar = \c -> isHexDigit c || ( isAlpha c && isLower c)
                hasHashtag = x == '#'
            in all isValidChar xs && length xs == 6 && hasHashtag

        ecls :: [String]
        ecls = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

        isValid :: Passport -> Bool
        isValid p = isJust $ do
            -- Check birth year
            byr <- readMaybe =<< M.lookup "byr" p
            guard (isInRange (1920, 2002) byr)
            -- Check issue year
            iyr <- readMaybe =<< M.lookup "iyr" p
            guard (isInRange (2010, 2020) iyr)
            -- Check expiration year
            eyr <- readMaybe =<< M.lookup "eyr" p
            guard (isInRange (2020, 2030) eyr)
            -- Check height
            hgt <- M.lookup "hgt" p
            let (height, unit) = splitAt (length hgt - 2) hgt
            intHeight <- readMaybe height
            let validHeight = case unit of
                    "cm" -> isInRange (150, 193) intHeight
                    "in" -> isInRange (59, 76) intHeight
                    _ -> False
            guard validHeight
            -- Check hair colour
            hcl <- M.lookup "hcl" p
            let validHcl = isValidHcl hcl
            guard validHcl
            -- Check eye colour
            ecl <- M.lookup "ecl" p
            let validEcl = ecl `elem` ecls
            guard validEcl
            -- Check passport ID
            pid <- M.lookup "pid" p
            let validPid = length pid == 9 && all isDigit pid
            guard validPid

day04a :: [Passport] :~> Int
day04a = Solution
    { parse = parseToMap . parseToList
    , solve = length . validatePassport
    , printer = show
    }

day04b :: [Passport] :~> Int
day04b = Solution
    { parse = parseToMap . parseToList
    , solve = length . validatePassportStrict . validatePassport
    , printer = show
    }