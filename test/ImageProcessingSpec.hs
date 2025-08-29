module ImageProcessingSpec (spec) where

import Codec.Picture (Pixel8, PixelRGB8 (..))
import ImageProcessing (averageColour)
import Test.Hspec

spec :: Spec
spec = do
  describe "Averaging colours" $ do
    -- sqrt(5 * (100^2) / 5)
    it "can average blues" $ do
      result `shouldBe` PixelRGB8 0 0 100
  where
    quiteBlue = PixelRGB8 0 0 100
    blues = replicate 5 quiteBlue
    result = averageColour blues
