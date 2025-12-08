{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day8 (run_parts,part1,part2) where

import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import Data.List (transpose,sort)
import Control.Monad (guard)
import qualified Data.List as List
import qualified Lib as L
import Data.Map (Map)
import qualified Data.Map as Map

tuple2Vec3 ::[Integer] -> L.IVec3
tuple2Vec3 (x:y:z:[]) = L.fromXYZ (x,y,z) 
tuple2Vec3 _ = error "invalid list length for tuple2Vec3"

sourceFile :: [L.IVec3]
sourceFile =  let 
                lines = T.lines $ L.decodeTextFile $(embedFile "data/2025_day8_p1.txt")
                tuples = map (T.splitOn ",") lines
                ints = map (map (read . T.unpack)) tuples
              in map tuple2Vec3 ints

edges :: [L.IVec3] -> [(Double, (L.IVec3, L.IVec3))]
edges verts = List.sortOn fst $ do 
                            (idx, v1) <- zipWith (,) [0..] verts
                            v2 <- drop (idx + 1) verts
                            guard (v1 /= v2)
                            return (L.norm (v1 - v2), (v1,v2))


part1 :: Int
part1 = let 
            edgesList = edges sourceFile
            takeEdges = take 1000 edgesList
            connectionPairs = map snd takeEdges
            finalCircuits = foldl (\circuits (v1,v2) -> L.connectNodes v1 v2 circuits) L.emptyGraph connectionPairs
            allCircuits = map (\(_,v) -> length v) $ Map.toList $ L.adjacencyMap finalCircuits
            values = List.sort allCircuits
         in product $ take 3 $ reverse values


getLastPair :: L.AdjacencyGraph L.IVec3 -> [(L.IVec3, L.IVec3)] -> (L.IVec3, L.IVec3)
getLastPair graph [] = error "no pairs left"
getLastPair graph (pair:rest) = let
                                    (v1,v2) = pair
                                    newGraph = L.connectNodes v1 v2 graph
                                in if L.numSubGraphs newGraph == 1 then 
                                    pair
                                   else
                                    getLastPair newGraph rest

part2 :: Integer
part2 = let 
            edgesList = edges sourceFile
            connectionPairs = map snd edgesList
            baseGraph = L.fromVertices sourceFile 
            lastPair@(lastV1, lastV2) = getLastPair baseGraph connectionPairs

         in (L.x lastV1) * (L.x lastV2) 

run_parts :: IO ()
run_parts = do
    print part1
    print part2