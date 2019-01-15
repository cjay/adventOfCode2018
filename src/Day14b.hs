{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VL

import Data.Word
import Data.Function
import Data.Maybe

import Control.Monad.State.Lazy
import Control.Monad.Identity
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts (oneShot)

wordlen = 6
chunkSize = 100000

-- | Irrefutable variant of splitAt. Makes an explicit spine for the prefix with
-- length n, allowing consumers like VL.fromListN to terminate on recursively
-- defined content
explicitSplit :: Int -> [a] -> ([a], [a])
explicitSplit n = make n where
  make 0 xs = ([], xs)
  make i (~(x:xs)) = (x : xs', rest) where
    (xs', rest) = make (i-1) xs

-- | Imitation of Debug.Trace.trace without the newline
traceStr :: String -> a -> a
traceStr s x = unsafePerformIO (putStr s >> hFlush stdout) `seq` x

stream :: [Word8]
stream = oneShot (concatMap VL.toList) chunks

-- tried replacing by fastIndex, but no speedup
getIndex :: Int -> Word8
getIndex ix = let
  (n, i) = ix `divMod` chunkSize
  chunk = chunks !! n
  in chunk VL.! i

fastIndex :: Int -> State ([VL.Vector Word8], VL.Vector (VU.Vector Word8)) Word8
fastIndex ix = do
  (chunks, chunkVec) <- get
  let (n, i) = ix `divMod` chunkSize
  if n < VL.length chunkVec then do
    return $ chunkVec VL.! n VU.! i
  else if n == VL.length chunkVec then do
    return $ (head chunks) VL.! i
  else do
    let (c:chunks') = chunks
        chunkVec' = VL.snoc chunkVec (VU.convert c)
    put (chunks', chunkVec')
    fastIndex ix

chunks :: [VL.Vector Word8]
-- chunks = consume $ evalState (generate fastIndex) (chunks, VL.empty) where
chunks = consume $ runIdentity (generate (pure . getIndex)) where
  consume xs = let (chunkContent, rest) = explicitSplit chunkSize xs
                   chunk = VL.fromListN chunkSize chunkContent
               in traceStr "." $ chunk : consume rest

generate :: Monad m => (Int -> m Word8) -> m [Word8]
generate getIndex = fmap ([3,7] ++) (gen [0, 1] 2) where
  gen cur n = do
    curVals <- mapM getIndex cur
    let sum_ :: Int = sum $ map fromIntegral curVals
        digits :: [Word8] = map (read . (:"")) $ show sum_
        n' = n + length digits
        cur' = map (`mod` n') $ zipWith (+) (map (fromIntegral . (+1)) curVals) cur
    fmap (digits ++) (gen cur' n')

searchL :: [Word8] -> [Word8] -> Maybe Int
searchL bs@(b:_) xs = go [] 0 xs where
  go _ _ [] = Nothing
  go !tries !n (x:xs) = let tries' = cont x tries in
    if [] `elem` tries' then Just n else go tries' (n+1) xs
  cont x tries = mapMaybe (match x) (bs:tries)
  match x (t:ts) | x == t    = Just ts
                 | otherwise = Nothing

main :: IO ()
main = do
  let input = 580741
  let inBytes = map (read . (:"") :: Char -> Word8) $ show input

  -- only part 2 in this variant
  print $ fromJust $ searchL inBytes stream