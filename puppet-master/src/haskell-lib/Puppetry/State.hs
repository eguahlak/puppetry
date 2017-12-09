{-# LANGUAGE DeriveGeneric #-}
module Puppetry.State where

import Puppetry.Color (Color)

import GHC.Generics (Generic)

-- | A single lamp
data Lamp = Lamp
  { lampColor :: Color
  , lampActive :: Bool
  } deriving (Show, Read, Generic)


-- | The internal state of the puppetry
data State = State
  {
  } deriving (Show, Read, Generic)
