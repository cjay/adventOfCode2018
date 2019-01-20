{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V

-- type VM = VM.IOVector Word8

data Tile = Goblin Int | Elf Int | Wall | Space



main :: IO ()
main = do
  print "foo"