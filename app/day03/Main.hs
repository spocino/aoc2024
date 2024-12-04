{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Main (module Main) where

import Data.Char (isNumber)
import Text.Regex.TDFA

data Command = Do | Dont | Mul Int Int

interpret :: Bool -> Bool -> Int -> [Command] -> Int
interpret ctrl cdo psum = \case
    []             -> psum
    (Do : cs)      -> interpret ctrl True psum cs
    (Dont : cs)    -> interpret ctrl False psum cs
    (Mul a b : cs) -> interpret ctrl cdo (psum + fromEnum (ctrl || cdo) * a * b) cs

parseInput :: IO [Command]
parseInput = do
    input <- readFile "inputs/day03.txt"
    let muls = getAllTextMatches $ input =~ "do\\(\\)|don't\\(\\)|mul\\([0-9]{1,3},[0-9]{1,3}\\)"
    return $ flip map muls \case
        "do()" -> Do
        "don't()" -> Dont
        tok -> let (a, b) = break (== ',') tok 
               in Mul (read $ filter isNumber a) (read $ filter isNumber b)

main :: IO ()
main = do
    muls <- parseInput
    putStrLn "Day 3: Mull It Over"
    let part1Solution = interpret True True 0 muls
    let part2Solution = interpret False True 0 muls
    putStrLn $ "\tPart 1: " <> show part1Solution
    putStrLn $ "\tPart 2: " <> show part2Solution

