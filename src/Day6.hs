module Day6 (run_parts,part1,part2) where

part1 :: Int
part1 = 42

part2 :: Int
part2 = 84

run_parts :: IO ()
run_parts = do
    print part1
    print part2