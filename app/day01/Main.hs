module Main (module Main) where

import Data.List (sort)

parseInput :: IO ([Int], [Int])
parseInput = do
    rawLines <- map words . lines <$> readFile "inputs/day01.txt"
    let pairs = [(read a :: Int, read b :: Int) | [a, b] <- rawLines]
    return (map fst pairs, map snd pairs)

calculateDifference :: ([Int], [Int]) -> Int
calculateDifference (as, bs) = sum [abs (a - b) | (a, b) <- zip (sort as) (sort bs)]

calculateSimilarity :: ([Int], [Int]) -> Int
calculateSimilarity (as, bs) = sum [a | a <- as, b <- bs, a == b]

main :: IO ()
main = do
    putStrLn "Day 1: Historian Hysteria"
    input <- parseInput
    putStrLn $ "\tPart 1: " <> show (calculateDifference input)
    putStrLn $ "\tPart 2: " <> show (calculateSimilarity input)

