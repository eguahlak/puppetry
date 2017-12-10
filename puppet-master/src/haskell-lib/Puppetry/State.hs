{-# LANGUAGE DeriveGeneric #-}
module Puppetry.State where

import Puppetry.Color (Color, cRed, cBlue, cGreen)

import GHC.Generics (Generic)

import Data.Aeson

import Data.Map (Map, empty, fromList)

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


emptyState :: State
emptyState =
  State empty empty empty empty empty empty

exampleState :: State
exampleState =
  emptyState
  { middle = fromList [ (0, cRed) , (13, cBlue), (25, cGreen) ]
  , front = fromList [ (0, cRed), (25, cBlue) ]
  , right = fromList [ (3, cRed) ]
  , left = fromList [ (2, cBlue) ]
  , procenium = fromList [ (11, cGreen )]
  }


instance FromJSON State

instance ToJSON State where
  toEncoding = genericToEncoding defaultOptions
