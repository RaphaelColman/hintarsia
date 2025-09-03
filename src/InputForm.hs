{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module InputForm where

import Codec.Picture (readImage)
import Codec.Picture.Types (DynamicImage)
import Control.Lens (makeLenses)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.IORef (readIORef)
import Text.Read (readMaybe)
import Validation (Validation (Failure, Success), failure, failureIf)

data Form = MkForm
  { _stitchGauge :: IORef String,
    _rowGauge :: IORef String,
    _numberOfColours :: IORef String,
    _targetStitches :: IORef String,
    _imageFilePath :: IORef String
  }

data ValidForm = MkValidForm
  { _validStitchGauge :: Double,
    _validRowGauge :: Double,
    _validNumberOfColours :: Int,
    _validTargetStitches :: Int,
    _validImage :: DynamicImage
  }

makeLenses ''ValidForm
makeLenses ''Form

data FormError
  = InvalidStitchGauge String
  | InvalidRowGauge String
  | InvalidNumberOfColours String
  | InvalidTargetStitches String
  | InvalidImageFilePath String
  deriving (Show, Eq)

validPositiveNumber :: err -> String -> Validation (NonEmpty err) Double
validPositiveNumber e input = case readMaybe @Double input of
  Just x -> x <$ failureIf (x <= 0) e
  Nothing -> failure e

validPositiveInteger :: err -> String -> Validation (NonEmpty err) Int
validPositiveInteger e input = case readMaybe @Int input of
  Just x -> x <$ failureIf (x <= 0) e
  Nothing -> failure e

validateStitchGauge :: String -> Validation (NonEmpty FormError) Double
validateStitchGauge = validPositiveNumber (InvalidStitchGauge "Must be a positive number")

validateRowGauge :: String -> Validation (NonEmpty FormError) Double
validateRowGauge = validPositiveNumber (InvalidRowGauge "Must be a positive number")

validateNumberOfColours :: String -> Validation (NonEmpty FormError) Int
validateNumberOfColours = validPositiveInteger (InvalidNumberOfColours "Must be a positive integer")

validateTargetStitches :: String -> Validation (NonEmpty FormError) Int
validateTargetStitches = validPositiveInteger (InvalidTargetStitches "Must be a positive integer")

validateImageFilePath :: String -> IO (Validation (NonEmpty FormError) DynamicImage)
validateImageFilePath path = do
  result <- readImage path
  case result of
    Left err -> return $ failure $ InvalidImageFilePath err
    Right img -> return $ Success img

validateForm :: Form -> IO (Validation (NonEmpty FormError) ValidForm)
validateForm form = do
  inputStitchGauge <- readIORef (_stitchGauge form)
  inputRowGauge <- readIORef (_rowGauge form)
  inputNumberOfColours <- readIORef (_numberOfColours form)
  inputTargetStitches <- readIORef (_targetStitches form)
  inputImageFilePath <- readIORef (_imageFilePath form)
  vDynamicImage <- validateImageFilePath inputImageFilePath
  pure $
    MkValidForm
      <$> validateStitchGauge inputStitchGauge
      <*> validateRowGauge inputRowGauge
      <*> validateNumberOfColours inputNumberOfColours
      <*> validateTargetStitches inputTargetStitches
      <*> vDynamicImage
