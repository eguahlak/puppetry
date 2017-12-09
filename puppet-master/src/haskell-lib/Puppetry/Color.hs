{-# LANGUAGE DeriveGeneric       #-}

module Puppetry.Color
  ( Color (..)
  , cBlack
  , cRed
  , cGreen
  , cBlue
  ) where

import GHC.Generics (Generic)

import Data.Binary.Put
import Data.Binary
import Data.Aeson (parseJSON, FromJSON, Value(..))

import           Numeric (readHex)
import           Control.Monad


import qualified Data.Text as T

data Color = Color
  { phase  :: !Word8
  , red   :: !Word8
  , green :: !Word8
  , blue  :: !Word8
  } deriving (Show, Read, Generic)

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
  parseJSON (String t) =
    case readHex (T.unpack t) of
      [(i, "")] ->
        return $ decode (runPut (putWord32be i))
      _ -> mzero
  parseJSON _ = mzero
