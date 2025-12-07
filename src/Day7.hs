{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day7 (run_parts,part1,part2) where

import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import Data.List (transpose,sort)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Lib as L

sourceFile :: Map (Int,Int) Char
sourceFile =  L.decodeCharacterGrid $ T.lines $ L.decodeTextFile $(embedFile "data/2025_day7_p1.txt")


-- walk down starting form a given location
-- if you encounter a '^' keep going from either side of "splitter"
-- cache paths along the way to avoid recomputation
-- return the location of every '^' encountered along the way
gridWalk :: Map (Int,Int) Char -> Set (Int,Int) -> Set (Int,Int) -> (Int,Int) -> (Set (Int,Int) ,Set (Int,Int))
gridWalk grid cache splitters currentLoc@(col,row) = 
    if Set.member currentLoc cache then (cache, splitters)
    else let
            nextSteps = gridStep grid currentLoc
            newSplitterSet = if length nextSteps == 2 then Set.insert (col,row+1) splitters else splitters
            (newCache, encounteredSplitters) = foldl (\(cach, splits) nextLoc -> gridWalk grid cach splits nextLoc
                                                     ) (Set.insert currentLoc cache, newSplitterSet) nextSteps
         in (newCache, encounteredSplitters)

-- if (col, row+1) is ('.') then return [(col, row+1)]
-- else if (col, row+1) is '^' then return (col-1, row+1) ++ (col+1, row+1)
-- unless col is at the edge of the grid or row is at the edge of the grid
gridStep :: Map (Int,Int) Char -> (Int,Int) -> [(Int,Int)]
gridStep grid currentLoc = let
                                (col, row) = currentLoc
                                belowLoc = (col, row + 1)
                                leftLoc = (col - 1, row + 1)
                                rightLoc = (col + 1, row + 1)
                            in case Map.lookup belowLoc grid of
                                Just '.' -> [belowLoc]
                                Just '^' -> [leftLoc, rightLoc]
                                _ -> []

locationOfS :: Map (Int,Int) Char -> (Int,Int)
locationOfS grid = head [ loc | (loc, c) <- Map.toList grid, c == 'S' ]


initialManyWorldsRow :: Map (Int,Int) Char -> Map (Int,Int) Integer
initialManyWorldsRow grid = let
                        sLoc = locationOfS grid
                        (maxCol, _) = L.gridLimits grid
                    in Map.insert sLoc 1 $ Map.fromList [ ((col,0), 0) | col <- [0..maxCol] ]

computeManyWorldsRow :: Map (Int,Int) Char -> Int -> Int -> Map (Int,Int) Integer -> Map (Int,Int) Integer
computeManyWorldsRow grid maxCol currRow previousComp = 
    let
        rowCols = [0..maxCol]
        newRowComp = foldl (\acc col -> let
                                            currLoc = (col, currRow)
                                            aboveLoc = (col, currRow -1)
                                            leftLoc = (col -1, currRow)
                                            rightLoc = (col +1, currRow)
                                            currentChar = Map.findWithDefault '.' currLoc grid
                                            aboveAmount = Map.findWithDefault 0 aboveLoc previousComp
                                        in case currentChar of
                                            '.' -> Map.insert currLoc (aboveAmount + Map.findWithDefault 0 currLoc acc) acc
                                            '^' -> Map.insert leftLoc (aboveAmount + Map.findWithDefault 0 leftLoc acc) $
                                                   Map.insert rightLoc (aboveAmount + Map.findWithDefault 0 rightLoc acc) acc
                                            _ -> acc
                             ) previousComp rowCols
    in newRowComp


computeAllRows :: Map (Int,Int) Char -> Map (Int,Int) Integer
computeAllRows grid = let
                            (maxCol, maxRow) = L.gridLimits grid
                            initialRow = initialManyWorldsRow grid
                            rows = [1..maxRow]
                            in foldl (\acc row -> computeManyWorldsRow grid maxCol row acc) initialRow rows

sumLastRow :: Map (Int,Int) Integer -> Integer
sumLastRow compGrid = let
                                (maxCol, maxRow) = L.gridLimits compGrid
                                rowCols = [0..maxCol]
                             in sum [ Map.findWithDefault 0 (col, maxRow) compGrid | col <- rowCols ]




part1 :: Int
part1 = let 
            startLoc = locationOfS sourceFile
            (visited, splitters) = gridWalk sourceFile Set.empty Set.empty startLoc
        in Set.size splitters

part2 :: Integer
part2 = let
            compGrid = computeAllRows sourceFile
        in sumLastRow compGrid

run_parts :: IO ()
run_parts = do
    print part1
    print part2