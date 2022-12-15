{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Puppetry.Color (
  Color (..),
  cBlack,
  cRed,
  cGreen,
  cBlue,
  cWhite,
  averageColor,
  dimColor,
) where

-- aseon
import Data.Aeson (
  FromJSON,
  ToJSON,
  defaultOptions,
  genericToEncoding,
  parseJSON,
  toEncoding,
  withObject,
  (.!=),
  (.:?),
 )

-- base
import Data.Binary as B
import Data.Monoid
import GHC.Generics (Generic)

data Color = Color
  { phase :: !Word8
  , red :: !Word8
  , green :: !Word8
  , blue :: !Word8
  }
  deriving (Show, Read, Generic, Eq)

cBlack :: Color
cBlack = Color{red = 0, green = 0, blue = 0, phase = 0}

cWhite :: Color
cWhite = Color{red = 255, green = 255, blue = 255, phase = 0}

cRed :: Color
cRed = cBlack{red = 255}

cGreen :: Color
cGreen = cBlack{green = 255}

cBlue :: Color
cBlue = cBlack{blue = 255}

dimColor :: Color -> Color
dimColor (Color r g b p) =
  Color (limit r) (limit g) (limit b) (limit p)
 where
  limit x =
    let y = fromIntegral x :: Double
     in round (min 255 (y * y / 256))

averageColor ::
  Foldable t =>
  t Color ->
  Color
averageColor f =
  let (getSum -> cnt, p, r, g, b) =
        foldMap
          ( \Color{..} ->
              ( Sum 1
              , Sum . fromIntegral $ phase
              , Sum . fromIntegral $ red
              , Sum . fromIntegral $ green
              , Sum . fromIntegral $ blue
              )
          )
          f
   in Color
        { phase = round ((getSum p :: Double) / cnt)
        , red = round ((getSum r :: Double) / cnt)
        , green = round ((getSum g :: Double) / cnt)
        , blue = round ((getSum b :: Double) / cnt)
        }

instance Binary Color

instance FromJSON Color where
  parseJSON = withObject "Color" $ \v ->
    Color
      <$> v .:? "phase" .!= 0
      <*> v .:? "red" .!= 0
      <*> v .:? "green" .!= 0
      <*> v .:? "blue" .!= 0

instance ToJSON Color where
  toEncoding = genericToEncoding defaultOptions
