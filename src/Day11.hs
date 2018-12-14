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

sumBlock :: Array Point Int -> Point -> Int
sumBlock arr p = sum $ map (arr!) $ range (p, p + pure 2)

findMaxBlock :: Array Point Int -> Point
findMaxBlock arr = maximumBy (compare `on` sumBlock arr) (range bounds_) where
    bounds_ = (V2 1 1, V2 (gridWidth-2) (gridHeight-2))

main :: IO ()
main = do
  let gridSerial = 5235 -- input
      grid = makeGrid gridSerial
      result = findMaxBlock grid
  print result
