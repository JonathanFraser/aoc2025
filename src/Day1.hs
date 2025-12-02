{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day1 (run_parts) where

import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import qualified Lib as L

fileLines :: [T.Text]
fileLines = T.lines $ L.decodeTextFile $(embedFile "data/2025_day1_p1.txt")

processLine :: String -> Int  
processLine ('R':x) = read x
processLine ('L':x) = negate (read x)

processLine2 :: String -> [Int]  
processLine2 ('R':x) = take (read x) (repeat 1)
processLine2 ('L':x) = take (read x) (repeat (-1))

increment :: Int -> Int -> Int
increment original step = (original + step) `mod` 100


part1 :: Int
part1 = let 
            steps = map (processLine . T.unpack) fileLines
            result = scanl increment 50 steps
        in length (filter (==0) result)

part2 :: Int
part2 = let 
            steps = map (processLine2 . T.unpack) fileLines
            result = scanl increment 50 (concat steps)
        in length (filter (==0) result)

run_parts :: IO ()
run_parts = do
    print part1
    print part2