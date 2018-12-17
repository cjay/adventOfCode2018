{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, NamedFieldPuns, ApplicativeDo, PatternSynonyms #-}
module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative
import Data.List
import Data.Array
import Data.Ord
import Linear.V2
import Control.Lens

newtype Pos = Pos { getPos :: V2 Int } deriving (Eq, Show, Ix)
type Vec = V2 Int

data Tile = Horiz | Vert | Slash | Backslash | Cross | Space deriving (Eq, Show)
data Dir = North | South | East | West deriving (Eq, Ord, Show)
data Turn = TurnLeft | Straight | TurnRight deriving (Eq, Ord, Show)
data Cart = Cart { cartDir :: !Dir, cartNextTurn :: !Turn, cartId :: !Int } deriving (Eq, Ord, Show)
data Collision = Collision { collNum :: !Int } deriving (Eq, Ord, Show)

-- Pos get sorted by row before column, which is also the move order of the carts
instance Ord Pos where
    compare = comparing ((^._yx) . getPos)

nextPos :: Pos -> Dir -> Pos
nextPos (Pos (V2 x y)) North = Pos $ V2 x (y-1)
nextPos (Pos (V2 x y)) South = Pos $ V2 x (y+1)
nextPos (Pos (V2 x y)) East = Pos $ V2 (x+1) y
nextPos (Pos (V2 x y)) West = Pos $ V2 (x-1) y

nextTurn :: Turn -> Turn
nextTurn TurnLeft = Straight
nextTurn Straight = TurnRight
nextTurn TurnRight = TurnLeft

updateCart :: Cart -> Tile -> Cart
updateCart Cart { cartDir, cartNextTurn, cartId } tile = Cart dir turn cartId where
    (dir, turn) = nextDir (cartDir, cartNextTurn) tile

-- should have gone with modulo arithmetic, would probably be less verbose
nextDir :: (Dir, Turn) -> Tile -> (Dir, Turn)

nextDir (North, _) Space = error "went north, hit space"
nextDir (North, _) Horiz = error "went north, hit horiz"
nextDir (North, t) Vert = (North, t)
nextDir (North, t) Slash = (East, t)
nextDir (North, t) Backslash = (West, t)
nextDir (North, t) Cross = (nextDir, nextTurn t) where
    nextDir = case t of TurnLeft -> West
                        Straight -> North
                        TurnRight -> East

nextDir (South, _) Space = error "went south, hit space"
nextDir (South, _) Horiz = error "went south, hit horiz"
nextDir (South, t) Vert = (South, t)
nextDir (South, t) Slash = (West, t)
nextDir (South, t) Backslash = (East, t)
nextDir (South, t) Cross = (nextDir, nextTurn t) where
    nextDir = case t of TurnLeft -> East
                        Straight -> South
                        TurnRight -> West

nextDir (East, _) Space = error "went east, hit space"
nextDir (East, t) Horiz = (East, t)
nextDir (East, _) Vert = error "went east, hit vert"
nextDir (East, t) Slash = (North, t)
nextDir (East, t) Backslash = (South, t)
nextDir (East, t) Cross = (nextDir, nextTurn t) where
    nextDir = case t of TurnLeft -> North
                        Straight -> East
                        TurnRight -> South

nextDir (West, _) Space = error "went west, hit space"
nextDir (West, t) Horiz = (West, t)
nextDir (West, _) Vert = error "went west, hit vert"
nextDir (West, t) Slash = (South, t)
nextDir (West, t) Backslash = (North, t)
nextDir (West, t) Cross = (nextDir, nextTurn t) where
    nextDir = case t of TurnLeft -> South
                        Straight -> West
                        TurnRight -> North


type Parser = Parsec Void Text

lineParser :: Int -> Parser ([(Pos, Tile)], [(Pos, Int -> Cart)])
lineParser row = do
  raw <- manyTill anySingle eol
  let numberedChars = zip [0..] raw
      tiles = flip map numberedChars $ \(col, c) ->
             let !tile = case c of '|' -> Vert
                                   '-' -> Horiz
                                   '/' -> Slash
                                   '\\' -> Backslash
                                   '+' -> Cross
                                   '>' -> Horiz
                                   '<' -> Horiz
                                   '^' -> Vert
                                   'v' -> Vert
                                   ' ' -> Space
                                   _ -> error $ "unexpected char " ++ show c ++ " at " ++ show (col, row)
              in (Pos (V2 col row), tile)
      carts = flip mapMaybe numberedChars $ \(col, c) ->
              let dir = case c of '>' -> Just East
                                  '<' -> Just West
                                  'v' -> Just South
                                  '^' -> Just North
                                  _ -> Nothing
              in dir >>= \dir' -> return (Pos (V2 col row), Cart dir' TurnLeft)
  return (tiles, carts)

sequenceTill :: Alternative m => m end -> [m a] -> m [a]
sequenceTill _ [] = pure []
sequenceTill end (a:as) = end *> pure [] <|> do
                            a' <- a
                            as' <- sequenceTill end as
                            return (a':as')

inputParser :: Parser (Array Pos Tile, [(Pos, Cart)])
inputParser = do
  lineResults <- sequenceTill eof $ map lineParser [0..]
  let (posWithTilesPerLine, posWithCartsPerLine) = unzip lineResults
      rows = length posWithTilesPerLine
      cols = maximum $ map length posWithTilesPerLine
      posWithTiles = concat posWithTilesPerLine
      posWithCarts = zipWith (\(pos, cart) cartId -> (pos, cart cartId)) (concat posWithCartsPerLine) [0..]
  return (array (Pos (V2 0 0), Pos (V2 (cols-1) (rows-1))) posWithTiles, posWithCarts)

type Object = Either Collision Cart

posObjsToCollisions :: [(Pos, Object)] -> [(Pos, Collision)]
posObjsToCollisions = mapMaybe $ \(pos, obj) ->
                      either (\coll -> Just (pos, coll)) (const Nothing) obj

posObjsToCarts :: [(Pos, Object)] -> [(Pos, Cart)]
posObjsToCarts = mapMaybe $ \(pos, obj) ->
                 either (const Nothing) (\cart -> Just (pos, cart)) obj

tick :: Array Pos Tile -> Map Pos Object -> Map Pos Object
tick tileArr !posObjs = foldl' move posObjs cartsInOrder where
    cartsInOrder = posObjsToCarts . sortOn fst $ Map.assocs posObjs
    move :: Map Pos Object -> (Pos, Cart) -> Map Pos Object
    move objMap (pos, cart) = if inCurrentPos /= Just (Right cart)
                              then objMap -- another cart ran into this cart already in the same tick
                              else Map.alter putCart pos' $ Map.delete pos objMap
                                   -- performance: could merge delete with pos lookup
      where
        inCurrentPos = Map.lookup pos objMap
        pos' = nextPos pos (cartDir cart)
        cart' = updateCart cart (tileArr ! pos')
        putCart Nothing = Just $ Right cart'
        putCart (Just (Right _)) = Just $ Left (Collision 2)
        ----- without cleanup of collisions:
        -- putCart (Just (Left (Collision n))) = Just $ Left (Collision (n+1))
        ----- a collision disappears as soon as a cart drives over it:
        putCart (Just (Left (Collision _))) = Just $ Right cart'

main :: IO ()
main = do
  input <- TIO.readFile "inputs/day13.txt"
  let (!tileArr, !carts) = either (\e -> error $ show e) id $ parse inputParser "" input
      objMapStates = iterate (tick tileArr) (Map.fromList . map (fmap Right) $ carts)
      posCollisions = concatMap (posObjsToCollisions  . Map.assocs) objMapStates
      posCartStates = map (posObjsToCarts . Map.assocs) objMapStates
      survivor = head $ fromJust $ find ((== 1) . length) posCartStates
  print $ head posCollisions -- 118,66
  print survivor -- 70,129
