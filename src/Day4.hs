{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day4 (run_parts,part1,part2) where   

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import qualified Lib as L

fileMap :: Map (Int,Int) Char
fileMap = L.decodeCharacterGrid $ T.lines $ L.decodeTextFile $(embedFile "data/2025_day4_p1.txt")

neighbors :: (Integral a, Integral b) => (a,b) -> [(a,b)]
neighbors (x,y) = [(x-1,y-1),(x,y-1),(x+1,y-1),
                      (x-1,y),(x+1,y),
                      (x-1,y+1),(x,y+1),(x+1,y+1)]


papercoords :: Map (Int,Int) Char -> [(Int,Int)]
papercoords m = map fst $ filter (\(_,c) -> c == '@') $ Map.toList m 

isAccessible :: Map (Int,Int) Char -> (Int,Int) -> Bool
isAccessible papLocs c = let 
                    neighborValues = map (\g -> Map.findWithDefault '.' g papLocs) $ neighbors c
                in length (filter (== '@') neighborValues) < 4

getAccessible :: Map (Int,Int) Char -> Set (Int,Int)
getAccessible papLocs = Set.fromList $ filter (isAccessible papLocs) (papercoords papLocs)


getAllAccessible :: Map (Int,Int) Char -> Set (Int,Int)
getAllAccessible papLocs = let
                                initialAccessible = getAccessible papLocs
                                newPapLocs = L.clearLocations papLocs initialAccessible
                            in if Set.null initialAccessible 
                               then Set.empty 
                               else Set.union initialAccessible (getAllAccessible newPapLocs)

part1 :: Int 
part1 = length $ getAccessible fileMap
            
part2 :: Int
part2 = length $ getAllAccessible fileMap

run_parts :: IO ()
run_parts = do
    print part1
    print part2