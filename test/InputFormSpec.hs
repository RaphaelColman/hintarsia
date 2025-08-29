module InputFormSpec (spec) where

import Test.Hspec
import Validation
import InputForm (validateForm)
import Data.List.NonEmpty (NonEmpty ((:|)))

spec :: Spec
spec = do
  describe "InputFormVaidation" $ do
    it "can validate stitch gauge" $ do
      pending
    it "can validate row gauge" $ do
      pending
    it "can validate number of colours" $ do
      pending
    it "can validate target stitches" $ do
      pending
    it "can validate image file path" $ do
      pending
