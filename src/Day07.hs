{-# LANGUAGE ScopedTypeVariables, TupleSections, LambdaCase #-}
module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Text.Parsec hiding (State)
import Text.Parsec.String
import Data.Char
import Data.Maybe

newtype Step = Step { stepToChar :: Char } deriving (Show, Ord, Eq)
step c | 'A' <= c && c <= 'Z' = Step c
       | otherwise            = error $ "Step Char out of range: " ++ [c]

data Dependency = Step :> Step
depTuple (a :> b) = (a, b)
condition = fst . depTuple
consequence = snd . depTuple

type ScoreMap = Map Step Int -- number of conditions for the step

depParser :: GenParser Char a [Dependency]
depParser = flip sepEndBy (char '\n') $ do
  string "Step "
  a <- upper
  string " must be finished before step "
  b <- upper
  string " can begin."
  return $ step a :> step b

solve :: [Dependency] -> [Step]
solve deps = go initialRoots initialScoreMap where
  initialRoots = Set.fromList (map condition deps) Set.\\ Set.fromList (map consequence deps)

  initialScoreMap :: ScoreMap
  initialScoreMap = Map.fromListWith (+) $ map ((,1) . consequence) deps

  depMap :: Map Step (Set Step)
  depMap = Map.fromListWith Set.union $ map (fmap Set.singleton . depTuple) deps

  go :: Set Step -> ScoreMap -> [Step]
  go roots scores = case Set.lookupMin roots of
                      Nothing -> [] -- empty set
                      Just next -> next : uncurry go (execute next roots scores)

  execute :: Step -> Set Step -> ScoreMap -> (Set Step, ScoreMap)
  execute next roots scores = foldl' update (Set.delete next roots, scores) consequences where
    consequences = maybe [] Set.toList $ Map.lookup next depMap
    update (rs, scs) step = let newCount = (scs Map.! step) - 1
                                scs' = Map.insert step newCount scs
                                rs' = if newCount == 0 then Set.insert step rs else rs
                            in (rs', scs')

stepTime :: Step -> Int
stepTime (Step c) = 60 + ord c - ord 'A' + 1

type Time = Int
data WorkerStatus = Idle | Working Time Step deriving Eq -- time is until step is finished
data State = State { time :: Time -- elapsed total time
                   , workers :: [WorkerStatus]
                   , roots :: Set Step
                   , scores :: ScoreMap }

-- returns: (new status after tick, maybe finished step to come into effect)
workerTick :: WorkerStatus -> (WorkerStatus, Maybe Step)
workerTick Idle = (Idle, Nothing)
workerTick (Working t s) = let t' = t - 1 in
                           if t' == 0 then (Idle, Just s) else (Working t' s, Nothing)

solveTime :: [Dependency] -> Int
solveTime deps = time $ go initialState where
  initialRoots = Set.fromList (map condition deps) Set.\\ Set.fromList (map consequence deps)

  initialScoreMap :: ScoreMap
  initialScoreMap = Map.fromListWith (+) $ map ((,1) . consequence) deps

  depMap :: Map Step (Set Step)
  depMap = Map.fromListWith Set.union $ map (fmap Set.singleton . depTuple) deps

  initialState = State (-1) (replicate 5 Idle) initialRoots initialScoreMap

  go :: State -> State
  go state = if null (roots state) && all (== Idle) (workers state)
             then state
             else go $ State time' workers'' roots'' scores' where
      time' = time state + 1
      (workers', execSteps) = fmap catMaybes $ unzip $ map workerTick (workers state)
      (roots', scores') = foldl' (flip execute) (roots state, scores state) execSteps
      (idleWs, busyWs) = partition (\case Idle -> True; _ -> False ) workers'
      (roots'', assignedWs) = assignWorkers roots' idleWs
      workers'' = busyWs ++ assignedWs

  execute :: Step -> (Set Step, ScoreMap) -> (Set Step, ScoreMap)
  execute next (roots, scores) = foldl' update (roots, scores) consequences where
    consequences = maybe [] Set.toList $ Map.lookup next depMap
    update (rs, scs) step = let newCount = (scs Map.! step) - 1
                                scs' = Map.insert step newCount scs
                                rs' = if newCount == 0 then Set.insert step rs else rs
                            in (rs', scs')

  assignWorkers roots [] = (roots, [])
  assignWorkers roots idleWs@(_:restWs) = case Set.lookupMin roots of
                                                 Nothing -> (roots, idleWs)
                                                 Just next -> pick next
    where pick next = (leftoverRoots, assignedWs ++ [w])
            where (leftoverRoots, assignedWs) = assignWorkers (Set.delete next roots) restWs
                  w = Working (stepTime next) next

main :: IO ()
main = do
  input <- readFile "inputs/day07.txt"
  let deps = either (error.show) id $ parse depParser "" input
      steps = solve deps
  putStrLn $ map stepToChar steps
  print $ solveTime deps
