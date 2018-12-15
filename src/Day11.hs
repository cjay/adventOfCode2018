module Main where

import Linear.V2
import Data.Array.Unboxed
import Data.List
import Data.Function (on)
import Debug.Trace (trace)
import Control.Parallel.Strategies

type Point = V2 Int

gridWidth = 300
gridHeight = 300

pwrLvl :: Int -> Point -> Int
pwrLvl gridSerial (V2 x y) = l2 where
    rackId = x + 10
    l0 = (rackId * y + gridSerial) * rackId
    l1 = (l0 `div` 100) `mod` 10
    l2 = l1 - 5

makeGrid :: Int -> UArray Point Int
makeGrid gridSerial = listArray bounds_ (map (pwrLvl gridSerial) (range bounds_)) where
    bounds_ = (V2 1 1, V2 gridWidth gridHeight)

sumBlock :: UArray Point Int -> Int -> Point -> Int
sumBlock arr size p = sum $ map (arr!) $ range (p, p + pure (size - 1))

boundsBySize :: Int -> (Point, Point)
boundsBySize size = (V2 1 1, V2 (gridWidth - size + 1) (gridHeight - size + 1))

maxBlockForSize :: UArray Point Int -> Int -> ((Point, Int), Int)
maxBlockForSize arr size = trace ("maxBlockForSize " ++ show size) $
                           maximumBy (compare `on` snd) $
                           map (\p -> ((p, size), sumBlock arr size p)) (range $ boundsBySize size)

-- | actually fast enough with -O2 and parallelization. with a good cpu at least :)
maxBlockNaive :: UArray Point Int -> ((Point, Int), Int)
maxBlockNaive arr = maximumBy (compare `on` snd) (map (maxBlockForSize arr) [1..300] `using` parList rdeepseq)

{-
maxBlock via powerGrid is much slower than maxBlockNaive. epic fail.

better approaches probably:
- add/subtract rows or columns for a sliding sum. Should probably slide in columns so it can add/subtract rows,
  considering array memory layout
- reuse sum from smaller square and add the extra row and column
- combination of those two. sliding along rows (adding/subtracting columns) -> make sum of first sqare based on smaller size

-----------------------------------
-}

-- | result: power (with edge len = 2^power) -> top left point of square -> sum of square with that edge length
powerGrid :: UArray Point Int -> Int -> Point -> Int
powerGrid baseArr = (arr!) where
    -- cache an array for each power of two edge length
    arr :: Array Int (Point -> Int)
    arr = listArray powerBounds $ map make (range powerBounds)

    maxPower = ceiling $ logBase 2 $ fromIntegral gridWidth
    powerBounds = (0, maxPower)

    make :: Int -> Point -> Int
    make 0 = \point@(V2 x y) -> if x > gridWidth || y > gridHeight then 0 else baseArr ! point
    make n = let sub = arr ! (n - 1)
                 len = 2 ^ (maxPower - n)
                 bounds_ = trace ("make "++show n++" len "++show len++" ") (V2 1 1, V2 len len)
                 gen (V2 x y) = sum $ map sub [V2 x' y' | x' <- [2*x-1, 2*x], y' <- [2*y-1, 2*y]] -- note pecularity of starting at 1
                 arrN :: UArray Point Int
                 arrN = listArray bounds_ $ map gen (range bounds_)
             in (arrN!)

maxBlock :: UArray Point Int -> ((Point, Int), Int)
maxBlock arr = maximumBy (compare `on` snd) (map maxForSize [1..300]) where
    pwrLook = powerGrid arr

    maxForSize :: Int -> ((Point, Int), Int)
    maxForSize size = trace ("maxForSize " ++ show size) $ maximumBy (compare `on` snd) blocksWithSize where
                      blocksWithSize = map (\p -> ((p, size), sumBlk size p)) (range $ boundsBySize size)

    sumBlk :: Int -> Point -> Int
    sumBlk size p = go startPwr bounds_ where
        startPwr = floor $ logBase 2 $ fromIntegral size
        bounds_ = (p, p + pure (size - 1))
        go pwr bnds@(V2 xMin yMin, V2 xMax yMax) = if xMin' > xMax' || yMin' > yMax' then go pwr' bnds
                                                   else inner + left + right + top + bottom where
          look = pwrLook pwr
          edgeLen = 2 ^ pwr
          (xMin'neg, xMinOffset) = (-xMin) `divMod` edgeLen
          xMin' = -xMin'neg
          (xMax', xMaxOffset) = xMax `divMod` edgeLen
          (yMin'neg, yMinOffset) = (-yMin) `divMod` edgeLen
          yMin' = -yMin'neg
          (yMax', yMaxOffset) = yMax `divMod` edgeLen
          pwr' = pwr - 1
          inner = sum $ map look [V2 x y | x <- [xMin'..xMax'], y <- [yMin'..yMax']]
          left = if xMinOffset > 0 then go pwr' (V2 xMin yMin, V2 (xMin + xMinOffset - 1) yMax) else 0
          right = if xMaxOffset > 0 then go pwr' (V2 (xMax - xMaxOffset + 1) yMin, V2 xMax yMax) else 0
          top = if yMinOffset > 0 then go pwr' (V2 (xMin + xMinOffset) yMin, V2 (xMax - xMaxOffset) (yMin + yMinOffset - 1)) else 0
          bottom = if yMaxOffset > 0 then go pwr' (V2 (xMin + xMinOffset) (yMax - yMaxOffset + 1), V2 (xMax - xMaxOffset) yMax) else 0


main :: IO ()
main = do
  let gridSerial = 5235 -- input
      !grid = makeGrid gridSerial
  print $ maxBlockForSize grid 3
  -- print $ maxBlock grid -- this is slower than maxBlockNaive. epic fail.
  print $ maxBlockNaive grid -- should be ((V2 232 289,8),79)
