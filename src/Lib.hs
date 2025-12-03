{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( decodeTextFile
    , splitCommaSeperated
    , splitRange
    , chunksOf
    , getGreedyOrderedMax
    , convertDigitsToInt
    ) where

import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.List as DL
import Data.FileEmbed (embedFile)

import Control.Exception (throw, Exception)

data AocException = InvalidInput String | UnexpectedError String deriving (Show)

instance Exception AocException

decodeTextFile :: BS.ByteString -> T.Text
decodeTextFile = E.decodeUtf8

splitCommaSeperated :: T.Text -> [T.Text]
splitCommaSeperated input = T.splitOn (T.pack ",") input

splitRange :: T.Text -> (Int, Int)
splitRange input = let
                    lower:upper:[] = T.splitOn (T.pack "-") input
                    readLower = read (T.unpack lower) :: Int
                    readUpper = read (T.unpack upper) :: Int
                  in (readLower, readUpper)

chunksOf :: Int -> T.Text -> [T.Text]
chunksOf n txt = if T.null txt then [] else let (first, rest) = T.splitAt n txt in first : chunksOf n rest


getGreedyOrderedMax :: Ord a => Int -> [a] -> [a]
getGreedyOrderedMax 1 values = [maximum values]
getGreedyOrderedMax n values = let 
                            maxValue = maximum (take (length values - n + 1) values)
                            idx = case DL.elemIndex maxValue values of
                                Nothing -> throw (UnexpectedError "maximum not found despite being previously found")
                                Just idx -> idx 
                            lowerMax = getGreedyOrderedMax (n-1) (drop (idx + 1) values)
                          in maxValue:lowerMax

-- head is MSB
convertDigitsToInt :: (Num a, Integral a) => [a] -> a
convertDigitsToInt digits = let
                                reverseValues = reverse digits
                                powers = [0..] 
                                zipped = zip reverseValues powers
                              in sum $ fmap (\(digit, power) -> digit * (fromIntegral 10)^power) zipped