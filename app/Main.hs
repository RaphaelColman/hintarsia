module Main (main) where

import Lib
import GtkApp

main :: IO ()
main = do
  mainGtkApp
  -- avgGrid <- produceColourGrid
  -- doDisplay avgGrid
