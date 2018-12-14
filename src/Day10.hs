module Main where

import qualified Data.Text.IO as TIO
import Text.Parsec hiding (State)
import Text.Parsec.Text
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Function (on)
import Linear.V2
import Linear.Metric
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

atTime :: Int -> PointInfo -> Point
atTime t (PointInfo pos vel) = pos + pure t * vel

boundingBox :: [Point] -> (Point, Point) -- top left, bottom right
boundingBox ps = (V2 xMin yMin, V2 xMax yMax) where
    xMax = maximum $ map (^._x) ps
    yMax = maximum $ map (^._y) ps
    xMin = minimum $ map (^._x) ps
    yMin = minimum $ map (^._y) ps

-- | convert Point to Int for IntSet storage
convert :: (Point, Point) -> Point -> Int
convert (topLeft, bottomRight) p = rowLen * y + x where
    V2 x y = p - topLeft
    rowLen = bottomRight^._x - topLeft^._x + 1

buildSet :: (Point -> Int) -> [Point] -> IntSet
buildSet conv ps = IntSet.fromList $ map conv ps

-- | counts vert and horiz neighbors for each pixel
score :: [Point] -> Int
score ps = let box = boundingBox ps
               conv = convert box
               set = buildSet conv ps
               neighbors (V2 x y) = [V2 (x-1) y, V2 (x+1) y, V2 x (y-1), V2 x (y+1)]
               pointScore p = sum (map (\n -> if IntSet.member (conv n) set then 1 else 0) (neighbors p))
           in sum $ map pointScore $ ps

-- | find time where the points are closest to each other
-- could also use derivative and do gradient descent properly
-- I'm not quite sure this terminates for every two points
closestTime :: PointInfo -> PointInfo -> Int
closestTime pi0@(PointInfo p0 _) pi1@(PointInfo p1 _) = go 0 where
    go t = if t == t' then t else go t' where
        p0' = atTime (t+1) pi0
        p1' = atTime (t+1) pi1
        dist = distance `on` fmap fromIntegral
        d0 = dist p0 p1
        d1 = dist p0' p1'
        t' = t + round (d1 / (d0 - d1))

render :: [Point] -> String
render ps =
  let (topLeft, bottomRight) = boundingBox ps
      ps' = map (subtract topLeft) ps
      xMax = bottomRight^._x - topLeft^._x
      yMax = bottomRight^._y - topLeft^._y
      conv = convert (V2 0 0, V2 xMax yMax)
      set = buildSet conv ps'
      line y = flip map [0 .. xMax] $ \x ->
               if (conv $ V2 x y) `IntSet.member` set then '#' else ' '
  in unlines $ map line [0 .. yMax]

main :: IO ()
main = do
  input <- TIO.readFile "inputs/day10.txt"
  let pointInfos = either (error.show) id $ parse pointParser "" input
      estimatedTime = closestTime (pointInfos!!0) (pointInfos!!1)
      candidateTimes = [(estimatedTime - 100) .. (estimatedTime + 100)]
      timeScore t = score . map (atTime t) $ pointInfos
      tWin = maximumBy (compare `on` timeScore) candidateTimes
      points = map (atTime tWin) $ pointInfos
  print tWin
  putStrLn $ render points
