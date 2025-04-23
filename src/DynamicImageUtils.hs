{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module DynamicImageUtils (Show) where

import Codec.Picture
  ( DynamicImage (..),
    Image (Image, imageData, imageHeight, imageWidth),
  )
import Text.Printf

instance Show DynamicImage where
  show :: DynamicImage -> String
  show (ImageY8 Image {..}) = printf "ImageY8, width: %d, height %d" imageWidth imageHeight
  show (ImageY16 Image {..}) = printf "ImageY8, width: %d, height %d" imageWidth imageHeight
  show (ImageY32 Image {..}) = printf "ImageY32, width: %d, height %d" imageWidth imageHeight
  show (ImageYF Image {..}) = printf "ImageYF, width: %d, height %d" imageWidth imageHeight
  show (ImageYA8 Image {..}) = printf "ImageYA8, width %d, height %d" imageWidth imageHeight
  show (ImageYA16 Image {..}) = printf "ImageYA16, width %d, height %d" imageWidth imageHeight
  show (ImageRGB8 Image {..}) = printf "ImageRGB8, width %d, height %d" imageWidth imageHeight
  show (ImageRGB16 Image {..}) = printf "ImageRGB16, width %d, height %d" imageWidth imageHeight
  show (ImageRGBF Image {..}) = printf "ImageRGBF, width %d, height %d" imageWidth imageHeight
  show (ImageRGBA8 Image {..}) = printf "ImageRGBA8, width %d, height %d" imageWidth imageHeight
  show (ImageRGBA16 Image {..}) = printf "ImageRGBA16, width %d, height %d" imageWidth imageHeight
  show (ImageYCbCr8 Image {..}) = printf "ImageYCbCr8, width %d, height %d" imageWidth imageHeight
  show (ImageCMYK8 Image {..}) = printf "ImageCMYK8, width %d, height %d" imageWidth imageHeight
  show (ImageCMYK16 Image {..}) = printf "ImageCMYK16, width %d, height %d" imageWidth imageHeight
