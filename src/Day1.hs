{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day1 (run_parts,part1,part2) where

import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import qualified Lib as L
import Control.Exception (throw, Exception)

data InvalidInput = InvalidInput String deriving (Show)

instance Exception InvalidInput

fileLines :: [T.Text]
fileLines = T.lines $ L.decodeTextFile $(embedFile "data/2025_day1_p1.txt")

processLine :: String -> Int  
processLine ('R':x) = read x
processLine ('L':x) = negate (read x)
processLine _ = throw (InvalidInput "Invalid input")

processLine2 :: String -> [Int]  
processLine2 ('R':x) = take (read x) (repeat 1)
processLine2 ('L':x) = take (read x) (repeat (-1))
processLine2 _ = throw (InvalidInput "Invalid input")

increment :: Int -> Int -> Int
increment original step = (original + step) `mod` 100

part1 :: Int
part1 = let 
            steps = map (processLine . T.unpack) fileLines
            result = scanl increment 50 steps
        in length (filter (==0) result)

part2 :: Int
part2 = let 
            steps = fileLines >>= (processLine2 . T.unpack)
            result = scanl increment 50 steps
        in length (filter (==0) result)

run_parts :: IO ()
run_parts = do
    print part1
    print part2