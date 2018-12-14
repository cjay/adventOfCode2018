module Main where

import Linear.V2
import Data.Array
import Data.List
import Data.Function (on)

type Point = V2 Int

gridWidth = 300
gridHeight = 300

pwrLvl :: Int -> Point -> Int
pwrLvl gridSerial (V2 x y) = l2 where
    rackId = x + 10
    l0 = (rackId * y + gridSerial) * rackId
    l1 = (l0 `div` 100) `mod` 10
    l2 = l1 - 5

makeGrid :: Int -> Array Point Int
makeGrid gridSerial = listArray bounds_ (map (pwrLvl gridSerial) (range bounds_)) where
    bounds_ = (V2 1 1, V2 gridWidth gridHeight)

sumBlock :: Array Point Int -> Int -> Point -> Int
sumBlock arr size p = sum $ map (arr!) $ range (p, p + pure (size - 1))

boundsBySize :: Int -> (Point, Point)
boundsBySize size = (V2 1 1, V2 (gridWidth - size + 1) (gridHeight - size + 1))

maxBlockForSize :: Array Point Int -> Int -> Point
maxBlockForSize arr size = maximumBy (compare `on` sumBlock arr size) (range $ boundsBySize size)

maxBlock :: Array Point Int -> (Point, Int)
maxBlock arr = maximumBy (compare `on` fst) $ map (\s -> (maxBlockForSize arr s, s)) [1..300]

main :: IO ()
main = do
  let gridSerial = 5235 -- input
      grid = makeGrid gridSerial
  print $ maxBlockForSize grid 3
  print $ maxBlock grid
