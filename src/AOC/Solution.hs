module AOC.Solution 
( (:~>)(..)
, showSolution
, runSolution
) where

{-|
    This type offers an abstracting for solving problems. 
    Solving problems always consists of three major steps:
        1. Parsing the input in some way
        2. Solving the problem
        3. Printing the result
-}
data a :~> b = Solution
    { parse :: String -> a      -- ^ Parser to parse String input to a
    , solve :: a -> b           -- ^ Function for solving the challange
    , output :: b -> String    -- ^ Function to print the result b
    }

-- |Runs a given solution with the appropiate input as String.
runSolution :: a :~> b -> String -> String
runSolution Solution{..} input =
    let x = parse input
        y = solve x
    in output y

-- |Shows the result of a given solution for given input
-- Input may be given as a String which represents the number of the
-- input text file. E.g. For input01.txt input "01"
showSolution :: a :~> b -> String -> IO ()
showSolution s input = do
    content <- readFile $ "input/input" ++ input ++ ".txt"
    let result = runSolution s content
    putStrLn result