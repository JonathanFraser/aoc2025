{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Day2 (run_parts) where

import Lib
import qualified Data.Text as T 
import qualified Lib as L
import Data.FileEmbed (embedFile)


ranges :: [T.Text]
ranges = L.splitCommaSeperated $ L.decodeTextFile $(embedFile "data/2025_day2_p1.txt")

getSkus :: T.Text -> [T.Text]
getSkus input = let 
                    (lower, upper) = L.splitRange input
               in fmap (T.pack . show) [lower .. upper]


checkInvalid :: Int -> T.Text -> Bool
checkInvalid split range = let
                                len = T.length range
                            in 
                                if len `mod` split /= 0 then False else 
                                    let 
                                        quant = len `div` split
                                        chunks = chunksOf quant range
                                        -- check all equal 
                                    in all (== head chunks) chunks

isInvalid1 :: T.Text -> Bool
isInvalid1 range = checkInvalid 2 range

isInvalid2 ::  T.Text -> Bool
isInvalid2 range = let 
                    len = T.length range
                    splitAmounts = [2..len]
                    invalids = map (\x -> checkInvalid x range) splitAmounts
                  in 
                    any (==True) invalids 

allskus :: [T.Text]
allskus = ranges >>= getSkus

part1 :: Int 
part1 = let 
            invalids1 = filter isInvalid1 allskus
         in sum $ fmap (read . T.unpack) invalids1

part2 :: Int
part2 = let
            invalids2 = filter isInvalid2 allskus
         in sum $ fmap (read . T.unpack) invalids2

run_parts :: IO ()
run_parts = do
    print part1
    print part2