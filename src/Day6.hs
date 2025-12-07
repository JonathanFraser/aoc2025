
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day6 (run_parts,part1,part2) where

import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import Data.List (transpose,sort)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Lib as L

sourceFile :: [T.Text]
sourceFile =  T.lines $ L.decodeTextFile $(embedFile "data/2025_day6_p1.txt")

numberMat = transpose $ map T.words sourceFile 

grid :: Map (Int,Int) Char
grid = L.decodeCharacterGrid sourceFile

operator :: T.Text -> Int -> Int -> Int
operator "*" a b = a * b
operator "+" a b = a + b

initial :: T.Text -> Int 
initial "*" = 1
initial "+" = 0

processLine :: [T.Text] -> Int
processLine values = let 
                        numbers = map read (T.unpack <$> init values) :: [Int]
                        op = operator (last values)
                    in foldl op (initial (last values)) numbers

digits = ['0'..'9']

readNumberColumn :: Map (Int, Int) Char -> Int -> Int -> Int
readNumberColumn grid maxRow col = read $ [0..maxRow] >>= \row -> case Map.lookup (col, row) grid of
                                        Just c -> if c `elem` digits then [c] else []
                                        Nothing -> []

operatorColumns :: Map (Int, Int) Char -> Int -> Int -> [Int]
operatorColumns grid maxRow maxCol = [0..maxCol] >>= \col -> case Map.lookup (col, maxRow) grid of
                                        Just c -> if c `elem` ['*','+'] then [col] else []
                                        Nothing -> []




operatorBlocks :: Map (Int, Int) Char -> Int -> Int -> [(Int, Int)]
operatorBlocks grid maxRow maxCol = let
                                        opCols = operatorColumns grid maxRow maxCol
                                        sortedOpCols = sort opCols 
                                        mostOps = zip sortedOpCols (map (\x -> x - 2) (tail sortedOpCols))
                                    in mostOps ++ [(last sortedOpCols, maxCol)]

cephlopodNumbers :: Map (Int, Int) Char -> [([Int],T.Text)]
cephlopodNumbers grid = let
                            (maxCol, maxRow) =  L.gridLimits grid
                            opBlocks = operatorBlocks grid maxRow maxCol
                        in map (\(startCol, endCol) -> 
                                    let
                                        numbers = [readNumberColumn grid maxRow col | col <- [startCol..endCol]]
                                        opChar = case Map.lookup (startCol, maxRow) grid of
                                                    Just c -> c
                                                    Nothing -> error "No operator found"
                                    in (numbers, T.pack [opChar])
                               ) opBlocks

processCephlopodLine :: ([Int], T.Text) -> Int
processCephlopodLine (numbers, op) = foldr (operator op) (initial op) numbers

part1 :: Int
part1 = sum $ map processLine numberMat

part2 :: Int
part2 = sum $ map processCephlopodLine numberBlocks
    where numberBlocks = cephlopodNumbers grid

run_parts :: IO ()
run_parts = do
    print part1
    print part2