{-|
Module      : AOC.Challenges.Day08
Description : Advent Of Code 2020 solutions for day 8 (https://adventofcode.com/2020/day/8)
Copyright   : (c) David Voigt, 2020
License     : MIT
-}
module AOC.Challenges.Day08
( day08a
, day08b
) where

import AOC.Solution ( (:~>)(..) )
import Data.List.Split (splitOn)
import qualified Data.Vector as V
import qualified Data.Set as Set
import Text.Read ( readMaybe )
import Control.Monad ( (>=>) )

data Op = NOP | ACC | JMP deriving (Show, Ord, Eq)
data Instr = Instr { operation :: Op, val :: !Int } deriving (Show)
data State = State { instrPtr :: !Int, acc :: !Int }

readInput :: String -> Maybe [Instr]
readInput = traverse getInstr . lines
    where
        readInstr :: String -> Maybe Op
        readInstr = \case
            "nop" -> Just NOP
            "acc" -> Just ACC
            "jmp" -> Just JMP
            _ -> Nothing 
        getInstr :: String -> Maybe Instr
        getInstr instr = case words instr of
            [instr, val] -> do
                instr' <- readInstr instr
                val' <- readMaybe $ filter (/= '+') val
                return (Instr instr' val')

-- PART 1

solve1 :: Maybe (V.Vector Instr) -> Maybe Int
solve1 vec = do
    instrs <- vec
    let go State{..} seen
            | Set.member instrPtr seen = Just acc
            | otherwise = case V.unsafeIndex instrs instrPtr of
                (Instr NOP _) -> go (State {instrPtr = instrPtr + 1, acc = acc}) (Set.insert instrPtr seen)
                (Instr ACC n) -> go (State {instrPtr = instrPtr + 1, acc = acc + n}) (Set.insert instrPtr seen)
                (Instr JMP n) -> go (State {instrPtr = instrPtr + n, acc = acc}) (Set.insert instrPtr seen)
    go (State 0 0) Set.empty

day08a :: Maybe (V.Vector Instr) :~> Maybe Int
day08a = Solution 
    { parse = readInput >=> \instrs -> return (V.fromList instrs)
    , solve = solve1
    , output = show
    }

-- PART 2

solve2 :: Maybe (V.Vector Instr) -> Maybe Int
solve2 vec = do
    instrs <- vec
    let
        replaceAt :: Int -> Maybe (V.Vector Instr)
        replaceAt idx = 
            let newInstr = case V.unsafeIndex instrs idx of
                    (Instr NOP n) -> Instr JMP n
                    i@(Instr ACC n) -> i
                    (Instr JMP n) -> Instr NOP n
            in return $ instrs V.// [(idx, newInstr)]
        tryAll :: Int -> Maybe Int
        tryAll idx
            | idx >= V.length instrs = Nothing
            | otherwise = case run (State 0 0) Set.empty =<< replaceAt idx of
                Just acc -> Just acc
                Nothing -> tryAll (idx + 1)
        run :: State -> Set.Set Int -> V.Vector Instr -> Maybe Int
        run State{..} seen instrs'
            | Set.member instrPtr seen = Nothing 
            | instrPtr >= V.length instrs' = Just acc
            | otherwise = case V.unsafeIndex instrs' instrPtr of
                (Instr NOP _) -> run (State {instrPtr = instrPtr + 1, acc = acc}) (Set.insert instrPtr seen) instrs'
                (Instr ACC n) -> run (State {instrPtr = instrPtr + 1, acc = acc + n}) (Set.insert instrPtr seen) instrs'
                (Instr JMP n) -> run (State {instrPtr = instrPtr + n, acc = acc}) (Set.insert instrPtr seen) instrs'
    tryAll 0

day08b :: Maybe (V.Vector Instr) :~> Maybe Int
day08b = Solution
    { parse = readInput >=> \instrs -> return (V.fromList instrs)
    , solve = solve2
    , output = show
    }