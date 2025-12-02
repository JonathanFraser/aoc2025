{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( decodeTextFile
    , splitCommaSeperated
    , splitRange
    , chunksOf
    ) where

import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.FileEmbed (embedFile)

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
