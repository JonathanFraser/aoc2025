{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day11 (run_parts,part1,part2) where

import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import Data.List (transpose,sort)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Lib as L

sourceFile :: Map (Int,Int) Char
sourceFile =  L.decodeCharacterGrid $ T.lines $ L.decodeTextFile $(embedFile "data/2025_day11_p1.txt")

part1 :: Int
part1 = 0

part2 :: Integer
part2 = 0

run_parts :: IO ()
run_parts = do
    print part1
    print part2