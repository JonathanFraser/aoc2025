{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day5 (run_parts,part1,part2) where

import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import qualified Lib as L
import Text.Read (readMaybe)

sourceFile :: [T.Text]
sourceFile =  T.lines $ L.decodeTextFile $(embedFile "data/2025_day5_p1.txt")


parseFile :: ([(Integer,Integer)],[Integer])
parseFile = let 
                (ranges, rest) = L.parseRangeBlock sourceFile
                (ints, _) = L.parseIntegerBlock rest
            in (ranges, ints)

inRange :: Integer -> (Integer,Integer) -> Bool
inRange val (lower,upper) = val >= lower && val <= upper

inRangeSet :: [(Integer,Integer)] -> Integer -> Bool
inRangeSet ranges val = any (inRange val) ranges

getFresh :: ([(Integer,Integer)],[Integer]) -> [Integer]
getFresh (ranges, vals) = filter (inRangeSet ranges) vals

fuse :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
fuse (bs, be) (ts, te) = 
                        if be < ts || te < bs then Nothing
                        else Just (min bs ts, max be te)

fuseAll :: [(Integer,Integer)] -> [(Integer,Integer)]
fuseAll [] = []
fuseAll (x:xs) = let 
                    allfused = map (fuse x) xs  
                    anyfused = any (/= Nothing) allfused
                    reduced = map (\(fs,v) -> case fs of 
                        Nothing -> v 
                        Just fused -> fused) $ zip allfused xs
                in if anyfused 
                   then fuseAll reduced
                   else x:(fuseAll xs)

segmentLength :: (Integer,Integer) -> Integer
segmentLength (s,e) = e - s + 1

getSize :: [(Integer,Integer)] -> Integer
getSize ranges = sum $ map segmentLength ranges

part1 :: Int
part1 = length $ getFresh parseFile

part2 :: Integer
part2 = fromIntegral $ getSize $ fuseAll $ fst parseFile

run_parts :: IO ()
run_parts = do
    print part1
    print part2