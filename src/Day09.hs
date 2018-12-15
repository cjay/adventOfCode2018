module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Parsec hiding (State)
import Text.Parsec.Text
import Data.Sequence hiding (length)
import qualified Data.Sequence as Seq
import Data.Array
import Data.List
import Data.Function (on)

intParser :: GenParser a Int
intParser = read <$> many1 digit

-- 405 players; last marble is worth 70953 points
marbleParser :: GenParser a (Int,  Int)
marbleParser = do
  players <- intParser
  string " players; last marble is worth "
  lastMarble <- intParser
  string " points"
  return (players, lastMarble)

type Marble = Int
type Player = Int
type Score = Int

data State = State { circle :: Seq Marble
                   , scores :: Array Player Score
                   , player :: Player    -- next player
                   , marble :: Marble }  -- next marble

-- current cursor one to the right
rotRight :: Seq a -> Seq a
rotRight Seq.Empty = Seq.Empty
rotRight (x :<| xs) = xs |> x

-- current cursor one to the left
rotLeft :: Seq a -> Seq a
rotLeft Seq.Empty = Seq.Empty
rotLeft (xs :|> x) = x <| xs

rotate :: Int -> Seq a -> Seq a
rotate 0 s = s
rotate n s = if n > 0
             then rotate (n-1) (rotRight s)
             else rotate (n+1) (rotLeft s)

initialState :: Int -> State
initialState players = State { circle = Seq.singleton 0
                             , scores = listArray (1, players) (repeat 0)
                             , player = 1 -- next player
                             , marble = 1 -- next marble
                             }

tick :: State -> State
tick st = st' { player = player st `mod` length (scores st) + 1 -- players start at 1
              , marble = marble st + 1 } where
  surprise = marble st `mod` 23 == 0
  st' = if not surprise
        then st { circle = (marble st) <| rotate 2 (circle st) }
        else st { circle = circle', scores = scores' } where
            rotated = rotate (-7) (circle st)
            taken :< circle' = viewl rotated
            scores' = scores st // [(player st, scores st ! (player st) + marble st + taken)]

runUntilMarble :: State -> Marble -> State
runUntilMarble st mrb = until (\s -> marble s > mrb) tick st

winner :: Array Player Score -> (Player, Score)
winner scs = maximumBy (compare `on` snd) (assocs scs)

main :: IO ()
main = do
  input <- TIO.readFile "inputs/day09.txt"
  let [l] = T.lines input
      Right (players, lastMarble) = parse marbleParser "" l
      st = runUntilMarble (initialState players) lastMarble
      st2 = runUntilMarble (initialState players) (lastMarble*100)
  print $ snd (winner (scores st))
  print $ snd (winner (scores st2))
