{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import Data.Maybe
import Data.Function (on)
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)
type Dist = Int
-- data Meta = Meta { metaDist :: Int, metaPlace :: Point }

dist :: Point -> Point -> Dist
dist a b = abs (fst a - fst b) + abs (snd a - snd b)

pointListBounds :: [Point] -> (Point, Point)
pointListBounds places = ((xMin, yMin), (xMax, yMax)) where
  xMax = maximum $ map fst places
  xMin = minimum $ map fst places
  yMax = maximum $ map snd places
  yMin = minimum $ map snd places

markBox :: [Point] -> Array Point (Maybe Point)
markBox places = listArray bounds (map closest (range bounds)) where
  bounds = pointListBounds places
  closest :: Point -> Maybe Point
  closest p = if dist p pl0 < dist p pl1 then Just pl0 else Nothing where
      pl0:pl1:_ = sortBy (compare `on` dist p) places

markedOnBorder :: Array Point (Maybe Point) -> Set Point
markedOnBorder box = Set.fromList (mapMaybe (box !) borderPts) where
    ((xMin, yMin), (xMax, yMax)) = bounds box
    borderPts = [(x, y) | x <- [xMin, xMax], y <- [yMin..yMax]] ++
                [(x, y) | y <- [yMin, yMax], x <- [(xMin+1) .. (xMax-1)]]

markBox2 :: [Point] -> Int
markBox2 places = length $ filter (< 10000) (map score (range bounds)) where
  bounds = pointListBounds places
  score p = sum (map (dist p) places)

main :: IO ()
main = do
  input <- readFile "inputs/day06.txt"
  let places :: [Point] = map (read . (\l -> "("++l++")")) (lines input)
      box = markBox places
      isOnBorder = flip Set.member (markedOnBorder box)
      counts :: Map Point Int
      counts = foldl' (\m p -> Map.insertWith (+) p 1 m) Map.empty (catMaybes $ elems box)
      validCounts = filter (not . isOnBorder . fst) (Map.assocs counts)
      (_, count) = maximumBy (compare `on` snd) validCounts
  print count
  print $ markBox2 places
