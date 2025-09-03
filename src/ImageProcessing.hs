{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ImageProcessing (AvgGrid, averageColour, colourGrid) where

import Codec.Picture
import Codec.Picture.Saving (imageToBitmap, imageToJpg)
import Control.Exception (throw)
import Control.Lens ((^.))
import Data.ByteString.Lazy as BS (writeFile)
import qualified Data.Map.Strict as M
import Data.Range
import Data.Word (Word8)
import Debug.Trace
import Linear
import StitchConfig
import Text.Printf (printf)
import Data.Foldable (minimumBy)
import Data.Function (on, (&))
import InputForm (ValidForm, validImage, validRowGauge, validStitchGauge, validTargetStitches, validNumberOfColours)
import Control.Lens.Operators ((.~))

type AvgGrid = M.Map (V2 Integer) PixelRGB8

colourGrid :: ValidForm -> (AvgGrid, StitchConfig)
colourGrid form = (reduceAvgGridToPalette avgGrid palette, sc)
  where di = form ^. validImage
        gauge = MkGauge (form ^. validStitchGauge, 1.0) (form ^. validRowGauge, 1.0)
        sc = getStitchConfig di gauge (toInteger (form ^. validTargetStitches))
        converted = convertRGB8 di
        grids = getGrids sc converted
        avgGrid = avgOverGrid grids
        palette = doPalettize (form ^. validNumberOfColours) converted

writeImageOut :: DynamicImage -> IO ()
writeImageOut di = do
  let bs = imageToBitmap di
  BS.writeFile "res/out.bmp" bs

{--
 - MkStitchConfig {_numStitches = 102, _numRows = 120, _stitchWidthInPixels = 5, _stitchHeightInPixels = 3}
 - So at stitch coord (2,2):
 - xStitches are [10,14] or [2*5, 2*5+4]
 - yStitches are [6,8] or (2*3, 2*3+2)
--}
getGrids :: (Pixel a) => StitchConfig -> Image a -> M.Map (V2 Integer) [a]
getGrids sc img = M.fromList tupList
  where
    coords = [V2 x y | x <- [0 .. (sc ^. numStitches - 1)], y <- [0 .. (sc ^. numRows - 1)]]
    pixelsFor (V2 x y) =
      let xMin = x * sc ^. stitchWidthInPixels
          yMin = y * sc ^. stitchHeightInPixels
       in [pixelAt img (fromInteger x) (fromInteger y) | x <- [xMin .. xMin + (sc ^. stitchWidthInPixels - 1)], y <- [yMin .. (yMin + sc ^. stitchHeightInPixels - 1)]]
    tupList = fmap (\c -> (c, pixelsFor c)) coords
    -- JuicyPixels has (0,0) as the top left corner, but we want (0,0) to be the bottom left corner
    flippedPixelAt img' x y = pixelAt img' x ((imageHeight img - 1) - y)

averageColour :: [PixelRGB8] -> PixelRGB8
averageColour xs = PixelRGB8 (avg rSquare) (avg gSquare) (avg bSquare)
  where
    (rSquare, gSquare, bSquare) =
      foldr
        ( \(PixelRGB8 r g b) (r', g', b') ->
            (r' + fromIntegral r ^ 2, g' + fromIntegral g ^ 2, b' + fromIntegral b ^ 2)
        )
        (0, 0, 0)
        xs
    numPixels = length xs
    avg x = round $ sqrt (fromIntegral x / fromIntegral numPixels)

avgOverGrid :: M.Map (V2 Integer) [PixelRGB8] -> M.Map (V2 Integer) PixelRGB8
avgOverGrid = M.map averageColour

doPalettize :: Int -> Image PixelRGB8 -> [PixelRGB8]
doPalettize numberOfColours image = [pixelAt palette x 0 | x <- [0 .. numberOfColours - 1]]
  where
    options = PaletteOptions MedianMeanCut False numberOfColours
    (_, palette) = palettize options image

-- TODO we could maybe do some type foo here that the number of colours is guaranteed by the type
reduceAvgGridToPalette :: AvgGrid -> [PixelRGB8] -> AvgGrid
reduceAvgGridToPalette grid palette = M.map pick grid
  where pick :: PixelRGB8 -> PixelRGB8
        pick pixel = minimumBy (compare `on` colourDistance pixel) palette

colourDistance :: PixelRGB8 -> PixelRGB8 -> Float
colourDistance colour1 colour2 = distance (toVector colour1) (toVector colour2)
  where toVector (PixelRGB8 r g b) = V3 (fromIntegral r) (fromIntegral g) (fromIntegral b)
