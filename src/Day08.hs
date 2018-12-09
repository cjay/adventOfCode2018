module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Data.Maybe

intParser :: GenParser a Int
intParser = read <$> many1 digit

nextInt :: GenParser a Int
nextInt = intParser <* optional space

data Node = Node { children :: [Node]
                 , metas :: [Int]
                 } deriving (Show)

nodeParser :: GenParser a Node
nodeParser = do
  nChildren <- nextInt
  nMetas <- nextInt
  children_ <- sequence $ replicate nChildren nodeParser
  metas_ <- sequence $ replicate nMetas nextInt
  return $ Node children_ metas_

metaSum :: Node -> Int
metaSum n = sum (metas n) + sum (map metaSum (children n))

value :: Node -> Int
value n | null (children n) = sum (metas n)
        | otherwise = sum $ map value specialChildren
        where specialChildren = mapMaybe select (metas n)
              select ix = let rest = drop (ix-1) (children n)
                          in if null rest then Nothing else Just $ head rest

main :: IO ()
main = do
  input <- TIO.readFile "inputs/day08.txt"
  let [l] = T.lines input
      Right tree = parse nodeParser "" l
  print $ metaSum tree
  print $ value tree
