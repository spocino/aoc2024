module Main (module Main) where

parseInput :: IO [[Int]]
parseInput = map (map read . words) . lines <$> readFile "inputs/day02.txt"

isSafe :: [Int] -> Bool
isSafe levels = do
    let diffs = zipWith (-) levels (tail levels)
    let isMonotonic = all (> 0) diffs || all (< 0) diffs
    let isGradual = all ((< 4) . abs) diffs
    isMonotonic && isGradual

withoutOne :: [a] -> [[a]]
withoutOne [] = []
withoutOne (x:xs) = xs : map (x :) (withoutOne xs)

main :: IO ()
main = do
    input <- parseInput
    putStrLn "Day 2: Red-Nosed Reports"
    let part1Solution = length $ filter isSafe input
    let part2Solution = length $ filter (any isSafe . withoutOne) input
    putStrLn $ "\tPart 1: " <> show part1Solution
    putStrLn $ "\tPart 2: " <> show part2Solution

