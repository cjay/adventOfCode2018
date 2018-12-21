{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V
import Data.Word
import Control.Monad
import Data.Function
import System.IO

type VM = VM.IOVector Word8

step :: VM -> ([Int], Int) -> IO ([Int], Int)
step v (cur, n) = do
  curVals <- mapM (VM.read v) cur
  let sum_ :: Int = sum $ map fromIntegral curVals
      digits :: [Word8] = map (read . (:"")) $ show sum_
  sequence_ $ zipWith (VM.write v) [n..] digits
  let n' = n + length digits
      cur' = map (`mod` n') $ zipWith (+) (map (fromIntegral . (+1)) curVals) cur
  return (cur', n')

wordlen = 6
chunkSize = 100000

work :: VM -> [Word8] -> Int -> ([Int], Int) -> IO Int
work v input len state@(_, n) = do
  if n >= len - 2 -- n can grow by 2 in each step
    then do
      let sliceStart = max 0 (n - chunkSize - wordlen)
      let sliceLen = n - sliceStart
      let sub = VM.slice sliceStart sliceLen v
      sub' <- V.freeze sub -- interestingly unsafeFreeze doesn't seem to provide any speedup
      let found = search sub' input
      case found of Just index -> return (sliceStart + index)
                    Nothing -> do
                      v' <- VM.grow v chunkSize
                      putStr "."
                      hFlush stdout
                      step v' state >>= work v' input (len + chunkSize)
    else do
      step v state >>= work v input len

-- it's not KMP, but probably fast enough
search :: V.Vector Word8 -> [Word8] -> Maybe Int
search _ [] = Nothing
search v bytes@(b:bs) = go (V.elemIndices b v) bs where
  len = V.length v
  go indices [] = if V.null indices then Nothing else Just (V.head indices - wordlen + 1)
  go indices (b:bs) = let indices' = V.filter (\i -> i < len && v V.! i == b) (V.map (+1) indices)
                      in go indices' bs

main :: IO ()
main = do
  let input = 580741
      curStart = [0,1] -- current positions of workers
      nStart = 2 -- amount of entries
      startLen = max (input * length curStart + nStart) (chunkSize + wordlen)
  v :: VM <- VM.new startLen
  VM.write v 0 3
  VM.write v 1 7
  (curStart, nStart) & fix (\loop (cur, n) -> when (n < input + 10) $ step v (cur, n) >>= loop)
  scores <- mapM (VM.read v ) [input .. input+9]
  print $ concat $ map show scores -- 6910849249
  let inBytes = map (read . (:"") :: Char -> Word8) $ show input
  index <- work v inBytes startLen (curStart, nStart)
  print index -- 20330673