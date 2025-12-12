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
import Data.Maybe (catMaybes)

sourceFile :: Map T.Text [T.Text]
sourceFile =  Map.fromList $ map processLine $ T.lines $ L.decodeTextFile $(embedFile "data/2025_day11_p1.txt")

processLine :: T.Text -> (T.Text,[T.Text])
processLine line =
    let parts = T.splitOn (T.pack ": ") line
        key = head parts
        values = T.words $ parts !! 1
    in (key, values)

computePossiblePaths :: PathReducer a => Map T.Text [T.Text] -> T.Text -> Set T.Text -> Map T.Text a -> Map T.Text a
computePossiblePaths connections source seen currentCounts = let 
    nextPoints = Map.findWithDefault [] source connections :: [T.Text]
    updatedCounts = foldl' (\x point -> computePossiblePaths connections point (Set.insert source seen) x) currentCounts nextPoints 
    current = catMaybes $ map (\x-> Map.lookup x updatedCounts) nextPoints
    reduced = reducePaths source current 
    in
        if Set.member source seen then currentCounts else -- loop drop this path
        if Map.member source currentCounts then currentCounts -- already computed
        else Map.insert source reduced updatedCounts

data PathTrace = PathTraceReducer (Map (Set T.Text) Int) deriving Show


filterValid :: Set T.Text -> PathTrace -> PathTrace
filterValid nodes (PathTraceReducer paths) = let
                                             pairs = Map.toList paths
                                             validSets = map (\(nodesSet,count) -> (Set.intersection nodesSet nodes,count)) pairs
                                             in PathTraceReducer $ Map.fromListWith (+) validSets
totalSum :: PathTrace -> Int
totalSum (PathTraceReducer paths) = sum $ Map.elems paths

addNode :: T.Text -> PathTrace -> PathTrace
addNode node (PathTraceReducer paths) = let
                                            allPaths = Map.toList paths
                                            newPaths = map (\(nodes,count) -> (Set.insert node nodes, count)) allPaths
                                            in PathTraceReducer $ Map.fromListWith (+) newPaths

updatePaths :: T.Text -> [PathTrace] -> [PathTrace]
updatePaths node paths = map (addNode node) paths

collapsePaths :: PathTrace -> PathTrace -> PathTrace
collapsePaths (PathTraceReducer p1) (PathTraceReducer p2) = PathTraceReducer $ Map.unionWith (+) p1 p2

collapseAllPaths :: [PathTrace] -> PathTrace
collapseAllPaths paths = foldr collapsePaths (PathTraceReducer Map.empty) paths

class PathReducer a where
    reducePaths :: T.Text -> [a] -> a


instance PathReducer Int where
    reducePaths t [] = if t == "out" then 1 else 0
    reducePaths _ paths = sum paths


instance PathReducer PathTrace where
    reducePaths t [] = if t == "out" then PathTraceReducer (Map.singleton (Set.singleton t) 1) else PathTraceReducer Map.empty
    reducePaths source paths = filterValid (Set.fromList ["out","dac","fft"]) $ collapseAllPaths $ updatePaths source paths

walkPaths :: PathReducer a => Map T.Text [T.Text] -> T.Text -> T.Text -> Map T.Text a
walkPaths connections source target = computePossiblePaths connections source (Set.empty) (Map.singleton target (reducePaths target []))
                   

part1 :: IO Int
part1 = do
            let precompute = walkPaths sourceFile "you" "out" :: Map T.Text Int
            let paths = Map.findWithDefault 0  "you" precompute
            return paths

part2 :: IO Int
part2 = do 
            let precompute = walkPaths sourceFile "svr" "out" :: Map T.Text PathTrace
            let pathsTrace = Map.findWithDefault (PathTraceReducer Map.empty) "svr" precompute
            let remainder =  Map.lookup (Set.fromList ["out","dac","fft"]) (let (PathTraceReducer m) = pathsTrace in m)
            return $ case remainder of
                Just count -> count
                Nothing -> 0


run_parts :: IO ()
run_parts = do
    result1 <- part1
    print result1
    print =<< part2