{-# LANGUAGE ScopedTypeVariables #-}
-- Until we export something specific
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Drawing where

import Codec.Picture
import qualified Data.Map as M
import Graphics.Gloss
import Linear.V2

cellSize = 10 -- For the moment. Maybe we can do something clever with window size

drawGrid :: M.Map (V2 Integer) PixelRGB8 -> Picture
drawGrid = undefined

doCell :: V2 Integer -> PixelRGB8 -> Picture
doCell (V2 x y) (PixelRGB8 r g b) = Color color cube
  where
    xMin :: Float = fromIntegral $ x * cellSize
    yMin :: Float = fromIntegral $ y * cellSize
    xMax :: Float = xMin + fromIntegral cellSize
    yMax :: Float = yMin + fromIntegral cellSize
    cube = Polygon [(xMin, yMin), (xMin, yMax), (xMax, yMax), (xMax, yMin)]
    color = makeColorI (fromIntegral r) (fromIntegral g) (fromIntegral b) 255

drawBox :: Picture
drawBox = boxPicture
  where
    box = Polygon [(0, 0), (0, -100), (-100, -100), (-100, 0)]
    boxPicture = Color red box

doDisplay = display (InWindow "Nice Window" (200, 200) (10, 10)) white drawBox
