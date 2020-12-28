{-|
Module      : AOC.Challenges.Day03
Description : Advent Of Code 2020 solutions for day 3 (https://adventofcode.com/2020/day/3)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day03
( day03a
, day03b
) where

import AOC.Solution ((:~>)(..))
import Data.Map as M ( insert, lookup, Map )

data Forest = Square | Tree deriving (Show, Eq)

type ForestMap = M.Map (Int, Int) Forest

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (x,y) (pX, pY) = ((x+pX) `mod` 31, y+pY) 

initMap :: [String] -> ForestMap
initMap xs = go (0, 0) xs mempty
    where
        go _ [] m                   = m
        go (x, y) (('.':ys):yss) m  = go (x+1, y) (ys:yss) (M.insert (x,y) Square m)
        go (x, y) (('#':ys):yss) m  = go (x+1, y) (ys:yss) (M.insert (x,y) Tree m)
        go (_,y) ([]:yss) m         = go (0, y+1) yss m

count :: ForestMap -> Int
count m = go (3,1) 0 m
    where
        go (x,y) n m =
            let ele = M.lookup (x,y) m
            in case ele of
                Nothing     -> n
                Just Tree   -> go (move (x,y) (3, 1)) (n+1) m
                Just _      -> go (move (x,y) (3, 1)) n m

count' :: ForestMap -> [Int]
count' m = do
    (dX, dY) <- [(1,1), (3,1), (5,1), (7, 1), (1,2)]
    go (dX, dY) (dX, dY) 0 m
    where
        go (x,y) (mX, mY) n m =
            let ele             = M.lookup (x,y) m
                (nextX, nextY)  = move (x,y) (mX, mY)
            in case ele of
                Nothing     -> [n]
                Just Tree   -> go (nextX, nextY) (mX, mY) (n+1) m
                Just _      -> go (nextX, nextY) (mX, mY) n m
    

day03a :: ForestMap :~> Int
day03a = Solution
    { parse = initMap . lines
    , solve = count
    , output = show
    }

day03b :: ForestMap :~> Int
day03b = Solution
    { parse = initMap . lines
    , solve = product . count'
    , output = show
    }