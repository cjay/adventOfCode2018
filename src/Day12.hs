{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Data.String
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Set (Set)
import Data.Maybe
import qualified Data.Set as Set

type Parser = Parsec Void Text

data Pot = Empty | Plant
           deriving (Eq, Ord, Show)
data Pattern = Pattern { l2::Pot, l1::Pot, cur::Pot, r1::Pot, r2::Pot }
               deriving (Eq, Ord, Show)
data Rule = Rule { pat::Pattern, next::Pot }
            deriving (Eq, Ord, Show)

potParser :: Parser Pot
potParser = do
  x <- char '#' <|> char '.'
  return $ if x == '#' then Plant else Empty

ruleParser :: Parser Rule
ruleParser = do
  -- I don't like that the 5 isn't compile time checked.
  -- Is there a better way other than writing it with five potParser lines?
  [l2,l1,cur,r1,r2] <- sequence $ replicate 5 potParser
  string " => "
  next <- potParser
  optional eol
  let pat = Pattern { l2, l1, cur, r1, r2 }
  return $ Rule { pat, next }


inputParser :: Parser ([Pot], [Rule])
inputParser = do
  string "initial state: "
  initialPots <- manyTill potParser newline
  newline
  rules <- many ruleParser
  return (initialPots, rules)

trim :: (Int, [Pot]) -> (Int, [Pot])
trim (startIndex, pots) = go startIndex pots where
    go sx (Empty:rest) = go (sx + 1) rest
    go sx rest = (sx, rest)

pad :: (Int, [Pot]) -> (Int, [Pot])
pad (startIndex, pots) = (startIndex - 4,
                          replicate 4 Empty ++ pots ++ replicate 4 Empty)

tick :: Set Pattern -> (Int, [Pot]) -> (Int, [Pot])
tick alivePats sxPots = trim . process . pad $ sxPots where
    process (n, pots) = (n + 2, go pots) -- go drops first two
    go (l2 : l1 : cur : r1 : r2 : rest) =
        let cur' = if (Pattern l2 l1 cur r1 r2) `Set.member` alivePats
                   then Plant else Empty
        in cur' : go (l1 : cur : r1 : r2 : rest)
    go rest = rest

main :: IO ()
main = do
  input <- TIO.readFile "inputs/day12.txt"
  let (initialPots, rules) = either (\e -> error $ show e) id $ parse inputParser "" input
      alivePats = Set.fromList $ map pat $ filter (\Rule { next } -> next == Plant) rules
      generations = iterate (tick alivePats) (0, initialPots)
      endGen = head $ drop 20 generations
      (startIndex, pots) = endGen
      potsWithPlants =
          catMaybes $ zipWith (\ix pot -> if pot == Plant then Just ix else Nothing)
                        [startIndex..] pots
  print $ sum potsWithPlants
