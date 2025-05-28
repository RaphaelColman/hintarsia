{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ImageProcessing (readImageIn, produceColourGrid, AvgGrid) where

import Codec.Picture
import Codec.Picture.Saving (imageToBitmap, imageToJpg)
import Control.Lens ((^.))
import Data.ByteString.Lazy as BS (writeFile)
import qualified Data.Map as M
import Data.Range
import Linear
import StitchConfig
import Text.Printf (printf)
import Data.Word (Word8)
import Control.Exception (throw)


type AvgGrid = M.Map (V2 Integer) PixelRGB8

-- Make this configurable as a cmd line argument
readImageIn :: IO ()
readImageIn = do
  image <- readImage "res/Haskell-Logo.svg.png"
  case image of
    Left err -> print err
    Right di -> do
      let sc = getStitchConfig di testGauge 100
      let converted = convertRGB8 di
      let grids = getGrids sc converted
      let avgd = avgOverGrid grids
      print $ avgd M.! (V2 25 50)
  pure ()

-- | This is not how it will work, I just need to produce a map
produceColourGrid :: IO (M.Map (V2 Integer) PixelRGB8)
produceColourGrid = do
  image <- readImage "res/Haskell-Logo.svg.png"
  case image of
    Left err -> undefined --TODO Either throw or convert the result into a Left (maybe ExceptT)
    Right di -> do
      let sc = getStitchConfig di testGauge 100
      let converted = convertRGB8 di
      let grids = getGrids sc converted
      pure $ avgOverGrid grids

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

averageColour :: [PixelRGB8] -> PixelRGB8
averageColour xs = PixelRGB8 (avg rSquare) (avg gSquare) (avg bSquare)
  where
    PixelRGB8 rSquare gSquare bSquare =
      foldr
        ( \(PixelRGB8 r g b) (PixelRGB8 r' g' b') ->
            PixelRGB8 (r + r' ^ 2) (g + g' ^ 2) (b + b' ^ 2)
        )
        (PixelRGB8 0 0 0)
        xs
    numPixels = length xs
    avg x = round $ sqrt (fromIntegral x / fromIntegral numPixels)

avgOverGrid :: M.Map (V2 Integer) [PixelRGB8] -> M.Map (V2 Integer) PixelRGB8
avgOverGrid = M.map averageColour
