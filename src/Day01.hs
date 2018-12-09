module Main where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

parse :: String -> Integer
parse l = case l!!0 of
            '+' -> read (tail l)
            _ -> (read l :: Integer)

day1 :: IO ()
day1 = do
  input <- readFile "inputs/day01.txt"
  let result = process input
  print result
    where
      process :: String -> Integer
      process contents = sum (map parse (lines contents))

day1b :: IO ()
day1b = do
  input <- readFile "inputs/day01.txt"
  let freqs = cycle (map (fromInteger.parse) (lines input))
  let states = scanl (+) 0 freqs
  let first [] _ = 0
      first (st:sts) set = if IS.member st set then st else first sts (IS.insert st set)
  let result = first states IS.empty
  print result

main :: IO ()
main = do
  day1
  day1b
