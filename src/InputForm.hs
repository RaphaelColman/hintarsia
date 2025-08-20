{-# LANGUAGE TemplateHaskell #-}

module InputForm
  ( ValidForm(MkValidForm),
    validImage,
    validRowGauge,
    validStitchGauge,
    validNumberOfColours,
    validTargetStitches,
  )
where

import Codec.Picture.Types (DynamicImage)
import Control.Lens (makeLenses)

data ValidForm = MkValidForm
  { _validStitchGauge :: Double,
    _validRowGauge :: Double,
    _validNumberOfColours :: Int,
    _validTargetStitches :: Int,
    _validImage :: DynamicImage
  }

makeLenses ''ValidForm
