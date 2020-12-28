module AOC.Challenges
( module Challenges
) where

import AOC.Solution
import AOC.Challenges.Day01 as Challenges ( day01a, day01b )
import AOC.Challenges.Day02 as Challenges ( day02a, day02b )
import AOC.Challenges.Day03 as Challenges ( day03a, day03b )
import AOC.Challenges.Day04 as Challenges ( day04a, day04b )
import AOC.Challenges.Day05 as Challenges ( day05a, day05b )
import AOC.Challenges.Day06 as Challenges ( day06a, day06b )
import AOC.Challenges.Day07 as Challenges ( day07a )


data Challenge a b = Challenge
    { solution  :: a :~> b
    , inputFile :: FilePath
    }