{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Data.List
import Data.Array

data Pos = Pos { posX :: Int
               , posY :: Int
               } deriving (Show, Eq, Ord, Ix)

data Size = Size { sizeWidth :: Int
                 , sizeHeight :: Int
                 } deriving (Show)

data Claim = Claim { claimId :: Int
                   , topLeft :: Pos
                   , claimSize :: Size
                   } deriving (Show)

intParser :: GenParser a Int
intParser = read <$> many1 digit

-- #123 @ 3,2: 5x4
claimParser :: GenParser a Claim
claimParser = do
  char '#'
  claimId <- intParser
  spaces
  char '@'
  spaces
  x <- intParser
  char ','
  y <- intParser
  char ':'
  spaces
  width <- intParser
  char 'x'
  height <- intParser
  eof
  let pos = Pos x y
      size = Size width height
  return $ Claim claimId pos size

touchPositions :: [Pos] -> Array Pos Int -> Array Pos Int
touchPositions ps arr = arr // map touch ps where
    touch pos = (pos, (arr ! pos) + 1)

markClaim :: Claim -> Array Pos Int -> Array Pos Int
markClaim claim arr = touchPositions ps arr where
    ps = range (topLeft claim, bottomRight claim)

mark :: [Claim] -> Array Pos Int
mark claims = foldl' (flip markClaim) emptyArr claims where
  bottomRights = map bottomRight claims
  maxX = maximum . map posX $ bottomRights
  maxY = maximum . map posY $ bottomRights
  arrBounds = (Pos 0 0, Pos maxX maxY)
  emptyArr = listArray arrBounds (repeat 0)

bottomRight :: Claim -> Pos
bottomRight (Claim _ pos size) = Pos (posX pos + sizeWidth size - 1) (posY pos + sizeHeight size - 1)

day3 claims = cnt where
    arr = mark claims
    cnt = length (filter (>1) (elems arr))

checkClaim :: Array Pos Int -> Claim -> Bool
checkClaim arr claim = all (== 1) (map (arr !) positions) where
    positions = range (topLeft claim, bottomRight claim)

day3b claims = filter (checkClaim arr) claims where
    arr = mark claims

main :: IO ()
main = do
  input <- TIO.readFile "inputs/day03.txt"
  let rawClaims = T.lines input
  let claims = either (error.show) id $ mapM (parse claimParser "") rawClaims
  print $ day3 claims
  print $ day3b claims
