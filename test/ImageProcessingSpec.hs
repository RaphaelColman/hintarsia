module ImageProcessingSpec (spec, sumWord8s) where

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

sumWord8s :: Pixel8 -> Pixel8 -> Integer
sumWord8s a b = fromIntegral a + fromIntegral b
