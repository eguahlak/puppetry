{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Puppetry.Color
  ( Color (..)
  , cBlack
  , cRed
  , cGreen
  , cBlue
  ) where

import           GHC.Generics (Generic)

import           Data.Aeson   (FromJSON, ToJSON, defaultOptions,
                               genericToEncoding, parseJSON, toEncoding,
                               withObject, (.!=), (.:?))
import           Data.Binary  as B

data Color = Color
  { phase :: !Word8
  , red   :: !Word8
  , green :: !Word8
  , blue  :: !Word8
  } deriving (Show, Read, Generic, Eq)

cBlack :: Color
cBlack = Color { red = 0, green = 0, blue = 0, phase = 0 }

cRed :: Color
cRed = cBlack { red = 255 }

cGreen :: Color
cGreen = cBlack { green = 255 }

cBlue :: Color
cBlue = cBlack { blue = 255 }

instance Binary Color

instance FromJSON Color where
  parseJSON = withObject "Color" $ \v -> Color
    <$> v .:? "phase" .!= 0
    <*> v .:? "red" .!= 0
    <*> v .:? "green" .!= 0
    <*> v .:? "blue" .!= 0

instance ToJSON Color where
  toEncoding = genericToEncoding defaultOptions
