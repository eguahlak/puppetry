{-# LANGUAGE DeriveGeneric #-}
module Puppetry.State where

import Puppetry.Color (Color)

import GHC.Generics (Generic)

import Data.Aeson

import Data.Map (Map)

-- | A single lamp
data Lamp = Lamp
  { lampColor :: Color
  , lampActive :: Bool
  } deriving (Show, Read, Generic)


-- | The internal state of the puppetry
data State = State
  { back   :: Map Int Color
  , middle :: Map Int Color
  , front  :: Map Int Color
  , left   :: Map Int Color
  , right  :: Map Int Color
  , procenium :: Map Int Color
  } deriving (Show, Read, Generic)


instance FromJSON State

instance ToJSON State where
  toEncoding = genericToEncoding defaultOptions
