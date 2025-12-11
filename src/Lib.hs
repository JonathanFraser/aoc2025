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
    , decodeCharacterGrid
    , clearLocation
    , clearLocations
    , neighbors
    , charInText
    , parseRangeBlock
    , parseIntegerBlock
    , gridLimits
    , AdjacencyGraph(..)
    , getNodeMembership
    , getNodeNeighbors
    , getOrInsertNode
    , connectNodes
    , emptyGraph
    , fromVertices
    , numSubGraphs
    , IVec3(..)
    , norm
    , fromXYZ
    , x
    , y
    , z
    , shoelace
    ) where

import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.List as DL
import Data.FileEmbed (embedFile)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Maybe (isJust)

import qualified Data.Set as Set

import Control.Exception (throw, Exception)

data AocException = InvalidInput String | UnexpectedError String deriving (Show)

instance Exception AocException

decodeTextFile :: BS.ByteString -> T.Text
decodeTextFile = E.decodeUtf8

splitCommaSeperated :: T.Text -> [T.Text]
splitCommaSeperated input = T.splitOn (T.pack ",") input

splitRange :: T.Text -> (Integer, Integer)
splitRange input = let
                    lower:upper:[] = T.splitOn (T.pack "-") input
                    readLower = read (T.unpack lower) :: Integer
                    readUpper = read (T.unpack upper) :: Integer
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



decodeCharacterGrid :: [T.Text] -> Map (Int,Int) Char
decodeCharacterGrid lines =  Map.fromList $ do 
                                    (y, line) <- zip [0..] lines
                                    (x, char) <- zip [0..] (T.unpack line)
                                    return ((x,y), char)

clearLocation :: (Integral k) => Map (k,k) Char -> (k,k) -> Map (k,k) Char
clearLocation papLocs loc = Map.insert loc '.' papLocs

clearLocations :: (Integral k) => Map (k,k) Char -> Set (k,k) -> Map (k,k) Char
clearLocations papLocs locs = Set.foldl' clearLocation papLocs locs

neighbors :: (Integral a, Integral b) => (a,b) -> [(a,b)]
neighbors (x,y) = [(x-1,y-1),(x,y-1),(x+1,y-1),
                      (x-1,y),(x+1,y),
                      (x-1,y+1),(x,y+1),(x+1,y+1)]

charInText :: Char -> T.Text -> Bool
charInText charToFind myText = isJust (T.find (== charToFind) myText)


parseRangeBlock :: [T.Text] -> ([(Integer,Integer)],[T.Text])
parseRangeBlock [] = ([],[])
parseRangeBlock (x:xs) = if x == T.pack "" then ([], xs)
                        else let 
                                (ranges, rest) = parseRangeBlock xs
                                newRange = splitRange x
                             in (newRange:ranges, rest)

parseIntegerBlock :: [T.Text] -> ([Integer],[T.Text])
parseIntegerBlock [] = ([],[])
parseIntegerBlock (x:xs) = if x == T.pack "" then ([], xs)
                        else let 
                                (ints, rest) = parseIntegerBlock xs
                                newInt = read (T.unpack x) :: Integer
                             in (newInt:ints, rest)

gridLimits :: Map (Int,Int) a -> (Int,Int)
gridLimits grid = let
                    cols  = map fst (Map.keys grid)
                    rows = map snd (Map.keys grid)
               in (maximum cols, maximum rows)


data AdjacencyGraph a = AdjacencyGraph {
    graphMembership :: Map a Integer, 
    adjacencyMap :: Map Integer (Set a)
} deriving (Show) 

fromVertices :: (Ord a) => [a] -> AdjacencyGraph a
fromVertices vertices = foldl (\graph v -> fst (getOrInsertNode v graph)) emptyGraph vertices

numSubGraphs :: AdjacencyGraph a -> Int
numSubGraphs graph = Map.size (adjacencyMap graph)

emptyGraph :: AdjacencyGraph a
emptyGraph = AdjacencyGraph Map.empty Map.empty

getNodeMembership :: (Ord a) => a -> AdjacencyGraph a -> Maybe Integer
getNodeMembership node graph = Map.lookup node (graphMembership graph)

getNodeNeighbors :: Integer -> AdjacencyGraph a -> Set a
getNodeNeighbors graphIdent graph = Map.findWithDefault Set.empty graphIdent (adjacencyMap graph)

getOrInsertNode :: (Ord a) => a -> AdjacencyGraph a -> (AdjacencyGraph a, Integer)
getOrInsertNode node graph = case Map.lookup node (graphMembership graph) of
    Just membership -> (graph, membership)
    Nothing -> let
                   newMembership = fromIntegral (Map.size (graphMembership graph))
                   newGraphMembership = Map.insert node newMembership (graphMembership graph)
                   newAdjacencyMap = Map.insert newMembership (Set.singleton node) (adjacencyMap graph)
                   newGraph = AdjacencyGraph newGraphMembership newAdjacencyMap
               in (newGraph, newMembership)

connectNodes :: (Ord a) => a -> a -> AdjacencyGraph a -> AdjacencyGraph a
connectNodes node1 node2 graph = let
                                    (graphWithNode1, membership1) = getOrInsertNode node1 graph
                                    (graphWithNode2, membership2) = getOrInsertNode node2 graphWithNode1
                                    neighbors1 = Map.findWithDefault Set.empty membership1 (adjacencyMap graphWithNode2)
                                    neighbors2 = Map.findWithDefault Set.empty membership2 (adjacencyMap graphWithNode2)
                                    mergedNeighbors = Set.union neighbors1 neighbors2
                                    newAdjacencyMap = Map.insert membership1 mergedNeighbors $
                                                      Map.delete membership2 (adjacencyMap graphWithNode2)
                                    newMembershipGraph = foldl (\m n -> Map.insert n membership1 m) (graphMembership graphWithNode2) (Set.toList mergedNeighbors)
                                  in AdjacencyGraph newMembershipGraph newAdjacencyMap

data IVec3 = IVec3 Integer Integer Integer deriving (Show, Eq, Ord)

instance Num IVec3 where
    (IVec3 x1 y1 z1) + (IVec3 x2 y2 z2) = IVec3 (x1 + x2) (y1 + y2) (z1 + z2)
    (IVec3 x1 y1 z1) - (IVec3 x2 y2 z2) = IVec3 (x1 - x2) (y1 - y2) (z1 - z2)
    (IVec3 x1 y1 z1) * (IVec3 x2 y2 z2) = IVec3 (x1 * x2) (y1 * y2) (z1 * z2)
    abs (IVec3 x y z) = IVec3 (abs x) (abs y) (abs z)
    signum (IVec3 x y z) = IVec3 (signum x) (signum y) (signum z)
    fromInteger n = IVec3 n n n

norm :: IVec3 -> Double
norm (IVec3 x y z) = sqrt (fromIntegral (x*x + y*y + z*z))

fromXYZ :: (Integral a) => (a,a,a) -> IVec3
fromXYZ (x,y,z) = IVec3 (fromIntegral x) (fromIntegral y) (fromIntegral z)

x :: IVec3 -> Integer
x (IVec3 a _ _) = a

y :: IVec3 -> Integer
y (IVec3 _ b _) = b

z :: IVec3 -> Integer
z (IVec3 _ _ c) = c

shoelace :: Num a => [(a,a)] -> a
shoelace vertices = let
                        wrapped = vertices ++ [head vertices]
                        pairs = zip vertices (tail wrapped)
                        crossProductsSum = sum [ (x1 * y2) - (x2 * y1) | ((x1, y1), (x2, y2)) <- pairs ]
                    in abs (crossProductsSum) 


data Rectangle = Rectangle {
    bottomLeft :: (Int, Int),
    topRight :: (Int, Int)
} deriving (Show, Eq)

area :: Rectangle -> Int
area rect = (x2 - x1 + 1) * (y2 - y1 + 1)
  where
    (x1, y1) = bottomLeft rect
    (x2, y2) = topRight rect

validateRectangle :: Rectangle -> Bool
validateRectangle rect = let
    (x1, y1) = bottomLeft rect
    (x2, y2) = topRight rect
  in x1 <= x2 && y1 <= y2

boarderPoints :: Rectangle -> [(Int, Int)]
boarderPoints rect = let
    (x1, y1) = bottomLeft rect
    (x2, y2) = topRight rect
    bottomEdge = [(x, y1) | x <- [x1..x2]]
    topEdge = [(x, y2) | x <- [x1..x2]]
    leftEdge = [(x1, y) | y <- [y1..y2]]
    rightEdge = [(x2, y) | y <- [y1..y2]]
  in Set.toList $ Set.fromList (bottomEdge ++ topEdge ++ leftEdge ++ rightEdge)
