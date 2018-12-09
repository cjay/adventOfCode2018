{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Function (on)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Token
import Data.List
import Data.Array
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Time.LocalTime
import Data.Time.Format

timeFormat = "%Y-%m-%d %H:%M"

data Event = Guard Int | Sleep | Wake deriving (Eq, Ord, Show)
data Entry = Entry { entryTime :: LocalTime
                   , entryEvent :: Event
                   } deriving (Eq, Ord, Show)

intParser :: GenParser a Int
intParser = read <$> many1 digit

timeParser :: GenParser a LocalTime
timeParser = do
  timeStr <- many (char '-' <|> char ':' <|> digit <|> space)
  return $ parseTimeOrError True defaultTimeLocale timeFormat timeStr

guardParser :: GenParser a Event
guardParser = do
  string "Guard #"
  guardId <- intParser
  string " begins shift"
  return (Guard guardId)

entryParser :: GenParser a Entry
entryParser = do
  time <- between (char '[') (char ']') timeParser
  spaces
  event <- guardParser <|>
           string "falls asleep" *> pure Sleep <|>
           string "wakes up" *> pure Wake
  return $ Entry time event

data SleepSpan = SleepSpan { start :: Int -- incl
                           , end :: Int   -- excl
                           }
type GuardMap = IntMap [SleepSpan]

sleepLen :: SleepSpan -> Int
sleepLen sleepSpan = end sleepSpan - start sleepSpan

process :: GuardMap -> [Entry] -> GuardMap
process gm [] = gm
process gm (Entry _ (Guard gId) : es) = process gm' restEs where
    (newSpans, restEs) = consumeSpans es
    gm' = IntMap.insertWith (++) gId newSpans gm
process _ (e:_) = error $ "entry of unexpected type: " ++ show e

consumeSpans :: [Entry] -> ([SleepSpan], [Entry])
consumeSpans es = go [] es where
  -- go spans restEs@(Entry _ (Guard _) : _)            = (spans, restEs)
  go spans (Entry t1 Sleep : Entry t2 Wake : restEs) = go (newSpan : spans) restEs
      where mins    = todMin . localTimeOfDay
            newSpan = SleepSpan (mins t1) (mins t2)
  go spans restEs = (spans, restEs)
  -- go _ (next : restEs) = error (show next)


minuteArr spans = foldl' touchSpan emptyArr spans where
  emptyArr = listArray (0, 59) (repeat 0)
  touchSpan arr sp = arr // updates where
    updates = flip map (range (start sp, end sp - 1)) $ \minute ->
              (minute, (arr ! minute) + 1)

main :: IO ()
main = do
  input <- TIO.readFile "inputs/day04.txt"
  let rawEntries = T.lines input
      entries = either (error.show) id $ mapM (parse entryParser "") rawEntries
      guardMap = process IntMap.empty (sort entries)
      totals = flip map (IntMap.toList guardMap) $ \(gId, sleepSpans) ->
              (gId, sum (map sleepLen sleepSpans))
      (winnerId, _) = maximumBy (compare `on` snd) totals
      winnerSpans = guardMap IntMap.! winnerId
      winnerMinuteAndCount spans = maximumBy (compare `on` snd) (assocs (minuteArr spans))
      (winnerMinute, _) = winnerMinuteAndCount winnerSpans
  print $ winnerId * winnerMinute
  let gIdMinuteCount = flip map (IntMap.toList guardMap) $ \(gId, sleepSpans) ->
                       (gId, winnerMinuteAndCount sleepSpans)
      (winnerId2, (winnerMinute2, _)) = maximumBy (compare `on` (snd.snd)) gIdMinuteCount
  print $ winnerId2 * winnerMinute2
