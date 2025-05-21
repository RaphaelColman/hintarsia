{-# LANGUAGE ScopedTypeVariables #-}
-- Until we export something specific
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Drawing where

import Codec.Picture
import qualified Data.Map as M
import Graphics.Gloss
import Linear.V2
import Data.Function ((&))

cellSize = 5 -- For the moment. Maybe we can do something clever with window size

--given coord x y, what is the cell location?
--if it's 2,3, and cellsize is 5 -> (10, 15) (or 2*5, 3*5)
drawGrid :: M.Map (V2 Integer) PixelRGB8 -> Picture
drawGrid mp = Pictures cells
  where
    cells = M.foldrWithKey toCell [] mp
    toCell :: V2 Integer -> PixelRGB8 -> [Picture] -> [Picture]
    toCell (V2 x y) color xs = newCell : xs
      where newCell = cellWithBorder (cellSize, cellSize) (x * cellSize, y * cellSize) color

cellWithBorder :: (Integer, Integer) -> (Integer, Integer) -> PixelRGB8 -> Picture
cellWithBorder (width, height) (x, y) (PixelRGB8 r g b) = translate (fromIntegral x) (fromIntegral y) $ Pictures [innerRect, border]
  where
    innerRect = rectangleSolid (fromIntegral width) (fromIntegral height) & Color color
    border = rectangleWire (fromIntegral width) (fromIntegral height)
    color = makeColorI (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

drawBox :: Picture
drawBox = drawGrid testGrid
  where
    cl = cellWithBorder (100, 100) (-50, -50) (PixelRGB8 255 0 0)

testGrid = M.fromList [(V2 0 0, PixelRGB8 255 0 0), (V2 1 1, PixelRGB8 0 255 0), (V2 (-1) (-1), PixelRGB8 0 0 255)]

doDisplay = display (InWindow "Nice Window" (200, 200) (10, 10)) white drawBox
