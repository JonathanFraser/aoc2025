{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day3 (run_parts) where

import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import qualified Lib as L
import qualified Data.List as DL

fileLines :: [T.Text]
fileLines = T.lines $ L.decodeTextFile $(embedFile "data/2025_day3_p1.txt")

getDigits :: T.Text -> [Int]
getDigits input = fmap (read . (:[])) (T.unpack input)


getMaxJoltage :: Int -> [Int] -> Int 
getMaxJoltage 1 digits = maximum digits
getMaxJoltage numdigits digits = let 
                            --maximum of first n digits
                            maxDigit = maximum (take (length digits - numdigits+1) digits)
                            --index of Maximum digit
                            Just idx  =  DL.elemIndex maxDigit digits 
                            --recurse with one less digit
                            lowerJoltage = getMaxJoltage (numdigits -1) (drop (idx + 1) digits)
                            in 10^(numdigits -1)*maxDigit + lowerJoltage

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
