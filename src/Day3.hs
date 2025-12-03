{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day3 (run_parts,part1,part2) where

import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import qualified Lib as L

fileLines :: [T.Text]
fileLines = T.lines $ L.decodeTextFile $(embedFile "data/2025_day3_p1.txt")

getDigits :: T.Text -> [Int]
getDigits input = fmap (read . (:[])) (T.unpack input)


getMaxJoltage :: Int -> [Int] -> Int 
getMaxJoltage 1 digits = maximum digits
getMaxJoltage numdigits digits = let 
                            values = L.getGreedyOrderedMax numdigits digits
                            in L.convertDigitsToInt values


getMaxPairJoltage :: [Int] -> Int 
getMaxPairJoltage digits = getMaxJoltage 2 digits

                                           
part1 :: Int 
part1 = let 
            digitsList = fmap getDigits fileLines
            maxPairs = fmap getMaxPairJoltage digitsList
         in sum maxPairs

part2 :: Int
part2 = let
            digitsList = fmap getDigits fileLines
            maxJoltages = fmap (getMaxJoltage 12) digitsList
         in sum maxJoltages

run_parts :: IO ()
run_parts = do 
    print part1
    print part2
