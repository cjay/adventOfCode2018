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

naiveIndex :: Int -> Word8
naiveIndex ix = let
  (n, i) = ix `divMod` chunkSize
  chunk = chunks !! n
  in chunk VL.! i

-- fastIndex without the unboxed chunks. same speed as naiveIndex, couldn't find out why
fastIndexBoxed :: Int -> State ([VL.Vector Word8], VL.Vector (VL.Vector Word8)) Word8
fastIndexBoxed ix = {-# SCC "FIB_all" #-} do
  (chunks, chunkVec) <- {-# SCC "FIB_get_state" #-} get
  let (n, i) = {-# SCC "FIB_divMod" #-} ix `divMod` chunkSize
      len = {-# SCC "FIB_length" #-} VL.length chunkVec
  {-# SCC "FIB_ifthenelse" #-} if {-# SCC "FIB_n_lt_len" #-} n < len then {-# SCC "FIB_normal_chunk_read" #-} do
    return $ chunkVec VL.! n VL.! i
  else if {-# SCC "FIB_n_eq_len" #-} n == len then {-# SCC "FIB_head_chunk_read" #-} do
    return $ (head chunks) VL.! i
  else {-# SCC "FIB_vector_append" #-} do
    let (c:chunks') = chunks
        chunkVec' = VL.snoc chunkVec c
    put (chunks', chunkVec')
    fastIndexBoxed ix

-- compared to naiveIndex and fastIndexBoxed. speedup only comes from the
-- unboxed chunk contents, not the indexing. something is wrong.
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
chunks = consume $ evalState (generate fastIndexBoxed) (chunks, VL.empty) where
-- chunks = consume $ runIdentity (generate (pure . naiveIndex)) where
  consume xs = let (chunkContent, rest) = explicitSplit chunkSize xs
                   chunk = VL.fromListN chunkSize chunkContent
               in traceStr "." $ chunk : consume rest

generate :: Monad m => (Int -> m Word8) -> m [Word8]
generate getIndex = fmap ([3,7] ++) (gen [0, 1] 2) where
  gen cur n = do
    curVals <- mapM getIndex cur
    let sum_ :: Word8 = sum $ map fromIntegral curVals
        -- only works for sum_ up to 99
        digits :: [Word8] = if a == 0 then [b] else [a,b] where
          (a, b) = sum_ `divMod` 10
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