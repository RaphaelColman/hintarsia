module Main (main) where

import Lib
import Graphics.Gloss
import Drawing
import ImageProcessing

main :: IO ()
main = do
  avgGrid <- produceColourGrid
  doDisplay avgGrid
