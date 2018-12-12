module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Parsec hiding (State)
import Text.Parsec.Text
import Data.Array
import Data.List
import Data.Function (on)
import Linear.V2
import Control.Lens

intParser :: GenParser a Int
intParser = do
  sign <- optionMaybe (char '-')
  ds <- many1 digit
  return $ read $ maybe ds (:ds) sign

type Point = V2 Int
type Vec = V2 Int

data PointInfo = PointInfo { position :: Point
                           , velocity :: Vec }

pointParser :: GenParser a [PointInfo]
pointParser = flip sepEndBy (char '\n') $ do
  string "position=<" <* spaces
  posX <- intParser <* string "," <* spaces
  posY <- intParser <* spaces <* string ">"
  spaces
  string "velocity=<" <* spaces
  velX <- intParser <* string "," <* spaces
  velY <- intParser <* spaces <* string ">"
  return $ PointInfo (V2 posX posY) (V2 velX velY)

atTime :: [PointInfo] -> Int -> [Point]
atTime [] _ = []
atTime (PointInfo pos vel : pis) t = pos + pure t * vel : atTime pis t

boundingBox :: [Point] -> (Point, Point) -- top left, bottom right
boundingBox ps = (V2 xMin yMin, V2 xMax yMax) where
    xMax = maximum $ map (^._x) ps
    yMax = maximum $ map (^._y) ps
    xMin = minimum $ map (^._x) ps
    yMin = minimum $ map (^._y) ps

render :: [Point] -> Array Point Bool
render ps = listArray box (repeat False) // map (\p -> (p, True)) ps where
    (topLeft, bottomRight) = boundingBox ps
    box = (topLeft - V2 1 1, bottomRight + V2 1 1) -- extra spacing so score doesn't need to check bounds

score :: [Point] -> Int
score ps = let arr = render ps
               neighbors (V2 x y) = [V2 (x+dx) (y+dy) | dx <- [-1, 1], dy <- [-1, 1]]
               pointScore p = sum (map (\n -> if arr!n then 1 else 0) (neighbors p))
           in sum $ map pointScore $ ps

main :: IO ()
main = do
  input <- TIO.readFile "inputs/day10.txt"
  let points = either (error.show) id $ parse pointParser "" input
      (tWin, sc) = maximumBy (compare `on` snd) $ map (\t -> (t, score . atTime points $ t)) [1..100]
  print tWin
