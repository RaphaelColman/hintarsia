{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module StitchConfig where

import Control.Lens (makeLenses, (^.))
import Control.Lens.Tuple (Field1 (_1), Field2 (_2))
import Linear (V2 (V2), _x)
import Codec.Picture

-- | Represents the target number of stitches and rows in the final stitch diagram
data StitchConfig = MkStitchConfig
  { _numStitches :: !Integer,
    _numRows :: !Integer,
    _stitchWidthInPixels :: !Integer,
    _stitchHeightInPixels :: !Integer
  }
  deriving (Show, Eq)

-- | Represents stitch size as ratio of (stitches, cm) and (rows, cm)
data Gauge = MkGauge
  { _stitchGauge :: (Double, Double),
    _rowGauge :: (Double, Double)
  }
  deriving (Show, Eq)

-- | The dimensions of a single stitch
type GaugeDimension = V2 Integer

makeLenses ''StitchConfig
makeLenses ''Gauge

-- | Image size -> Gauge dimensions -> Target no. of stitches
-- We want to guarantee that you get an integer number of pixels for each block size
calculatePatternSize :: V2 Integer -> GaugeDimension -> Integer -> StitchConfig
calculatePatternSize (V2 imageWidth imageHeight) (V2 gaugeX gaugeY) targetStitches =
  MkStitchConfig actualNumberOfStitches numberOfRows blockWidth blockHeight
  where
    blockWidth :: Integer = round (fromIntegral imageWidth / fromIntegral targetStitches)
    actualNumberOfStitches = imageWidth `div` blockWidth
    heightRatio :: Double = fromIntegral gaugeY / fromIntegral gaugeX
    blockHeight = round (heightRatio * fromIntegral blockWidth)
    numberOfRows = imageHeight `div` blockHeight

gaugeToGaugeDimension :: Gauge -> GaugeDimension
gaugeToGaugeDimension gauge = simplify $ V2 (f stitchGauge) (f rowGauge)
  where
    f g = round $ 1000 * gauge ^. (g . _2) / gauge ^. (g . _1) -- Eg if it's 4 stitches for 2 cm, then it's 2/4 = 0.5 cm per stitch

getStitchConfig :: DynamicImage -> Gauge -> Integer -> StitchConfig
getStitchConfig img gauge targetStitches =
  let width = dynamicMap imageWidth img
      height = dynamicMap imageHeight img
      imgDimens' = V2 (fromIntegral width) (fromIntegral height)
   in calculatePatternSize imgDimens' (gaugeToGaugeDimension gauge) targetStitches

testGauge :: Gauge
testGauge = MkGauge (2.1, 1.0) (3.4, 1.0) -- Taken from a nimble needles post

testGaugeDimension :: GaugeDimension
testGaugeDimension = gaugeToGaugeDimension testGauge

testStitchConfig :: StitchConfig
testStitchConfig = calculatePatternSize imgDimens testGaugeDimension 100
  where
    imgDimens = V2 512 362

simplify :: (Integral a) => V2 a -> V2 a
simplify (V2 x y) = let g = gcd x y in V2 (x `div` g) (y `div` g)
