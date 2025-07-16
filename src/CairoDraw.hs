module CairoDraw (drawGrid) where

import Codec.Picture (PixelRGB8 (..))
import Control.Lens ((^.))
import Control.Monad (foldM, foldM_, forM_)
import qualified Control.Monad
import Data.Foldable (foldrM)
import Data.Map as M (toList)
import Data.Word (Word8)
import Graphics.Rendering.Cairo
  ( Render,
    fillPreserve,
    rectangle,
    setLineWidth,
    setSourceRGB,
    stroke,
  )
import ImageProcessing (AvgGrid)
import Linear (V2 (..))
import StitchConfig (StitchConfig, stitchHeightInPixels, stitchWidthInPixels)

-- | Draws a grid of colored cells using Cairo.
-- (width, height), (x, y), and PixelRGB8 color.
drawCell :: (Integer, Integer) -> (Integer, Integer) -> PixelRGB8 -> Render ()
drawCell (width, height) (x, y) (PixelRGB8 r g b) = do
  setSourceRGB (toDouble r) (toDouble g) (toDouble b)
  setLineWidth 2.0
  rectangle (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
  fillPreserve
  setSourceRGB 0.0 0.0 0.0 -- Black border
  stroke
  where
    toDouble :: Word8 -> Double
    toDouble x = fromIntegral x / 255

drawGrid :: StitchConfig -> AvgGrid -> Render ()
drawGrid sc aGrid = do
  forM_
    (M.toList aGrid)
    ( \(V2 x y, color) -> do
        let width = sc ^. stitchWidthInPixels
            height = sc ^. stitchHeightInPixels
            xPos = x * width
            yPos = y * height
        drawCell (width, height) (xPos, yPos) color
    )
