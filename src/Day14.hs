{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Vector.Unboxed.Mutable as V
import Data.Word
import Control.Monad
import Data.Function

type V = V.IOVector Word8

step :: V -> ([Int], Int) -> IO ([Int], Int)
step v (cur, n) = do
  curVals <- mapM (V.read v) cur
  let sum_ :: Int = sum $ map fromIntegral curVals
      digits :: [Word8] = map (read . (:"")) $ show sum_
  sequence_ $ zipWith (V.write v) [n..] digits
  let n' = n + length digits
      cur' = map (`mod` n') $ zipWith (+) (map (fromIntegral . (+1)) curVals) cur
  return (cur', n')

main :: IO ()
main = do
  let input = 580741
  v :: V <- V.new $ input * 2 + 2
  V.write v 0 3
  V.write v 1 7
  ([0,1], 2) & fix (\loop (cur, n) -> when (n < input + 10) $ step v (cur, n) >>= loop)
  scores <- mapM (V.read v ) [input .. input+9]
  print $ concat $ map show scores -- 6910849249