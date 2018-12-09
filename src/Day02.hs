module Main where

import qualified Data.Set as Set
import Data.List
import Data.Maybe

-- run length encoding
compress :: String -> [(Char, Integer)]
compress "" = []
compress (x:xs) = go x 1 xs where
    go chr cnt [] = [(chr, cnt)]
    go chr cnt (y:ys) | chr == y  = go chr (cnt+1) ys
                      | otherwise = (chr, cnt) : go y 1 ys

count :: String -> (Bool, Bool)
count theId = (elem 2 counts, elem 3 counts) where
    sorted = sort theId
    counts = map snd (compress sorted)

day2 :: String -> Integer
day2 input = fromIntegral $ twice * thrice where
  ids = lines input
  counts = map count ids
  twice  = length (filter fst counts)
  thrice = length (filter snd counts)

findFirstDupe :: (Ord a) => [a] -> Maybe a
findFirstDupe xs = go xs Set.empty where
  go [] _ = Nothing
  go (y:ys) set = if Set.member y set then Just y else go ys (Set.insert y set)

killPos :: Int -> String -> String
killPos _ "" = ""
killPos n s = case splitAt n s of
                (pre, "") -> pre
                (pre, _:post) -> pre ++ post

day2b :: String -> String
day2b input = head results where
  ids = lines input
  columns = maximum (map length ids)
  killers = map killPos [0 .. (columns-1)]
  tryKiller k = findFirstDupe . map k $ ids
  results = mapMaybe tryKiller killers

main :: IO ()
main = do
  input <- readFile "inputs/day02.txt"
  print $ day2 input
  print $ day2b input
