{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Char (toLower)
import Data.Foldable (find, minimumBy)
import Data.Function (on)

canReact :: Char -> Char -> Bool
canReact a b = a /= b && toLower a == toLower b

step :: String -> String
step (a:b:rest) | canReact a b = step rest
                | otherwise = a : step (b:rest)
step rest = rest

-- dirty and slow solution to part 1
dirty :: String -> Int
dirty foo = a where
    ls = map length (iterate step foo)
    pairwise = zip ls (tail ls)
    Just (a, _) = find (uncurry (==)) pairwise

-- awesome and fast solution to part 1
process :: String -> String
process input = go [] input where
  -- first list: reversed, letters to the left of the cursor
  go (a:as) (b:bs) | canReact a b = go as bs
                   | otherwise = go (b:a:as) bs
  go [] (b:bs) = go [b] bs
  go as [] = reverse as

main :: IO ()
main = do
  input <- readFile "inputs/day05.txt"
  let [l] = lines input
  -- print $ dirty l
  print $ length (process l)

  let tries = map try ['a'..'z']
      try letter = (letter, res) where
        l' = filter ((/= letter) . toLower) l
        res = length (process l')
      (_, winnerLen) = minimumBy (compare `on` snd) tries
  print winnerLen
