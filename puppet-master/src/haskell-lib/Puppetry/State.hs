{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Puppetry.State where

import Puppetry.Color

import Data.Semigroup

import GHC.Generics (Generic)

import Data.Aeson

import qualified Data.Map as Map


-- | The internal state of the puppetry
data State = State
  { back   :: Strip
  , middle :: Strip
  , front  :: Strip
  , left   :: Strip
  , right  :: Strip
  , proscenium :: Strip
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
  , proscenium = fromList [ (11, cGreen )]
  }

instance FromJSON State

instance ToJSON State where
  toEncoding = genericToEncoding defaultOptions

newtype Strip = Strip { fromMap :: Map.Map Int Color }
 deriving (Show, Read, Generic)

empty :: Strip
empty = Strip $ Map.empty

fromList :: [(Int, Color)] -> Strip
fromList = Strip . Map.fromList

toList :: Strip -> [(Int, Color)]
toList = Map.toAscList . fromMap

average :: State -> Color
average State {..} =
  averageColor
  [ averageColor . fromMap $ back
  , averageColor . fromMap $ middle
  , averageColor . fromMap $ front
  , averageColor . fromMap $ left
  , averageColor . fromMap $ right
  , averageColor . fromMap $ proscenium
  ]


newtype ActiveLamp = ActiveLamp { toPair :: (Int, Color) }

instance FromJSON ActiveLamp where
  parseJSON = withObject "ActiveLamp" $ \v ->  (\a b -> ActiveLamp (a, b))
    <$> v .: "lamp"
    <*> v .: "color"

instance ToJSON ActiveLamp where
  toJSON (ActiveLamp (i, c)) =
    object [ "lamp" .= i, "color" .= c ]

  toEncoding (ActiveLamp (i, c)) =
    pairs ( "lamp" .= i <> "color" .= c )

instance FromJSON Strip where
  parseJSON v = do
    lamps <- parseJSON v
    return $ fromList (map toPair lamps)

instance ToJSON Strip where
  toJSON (Strip s) =
    toJSON (map ActiveLamp $ Map.toList s)

  toEncoding (Strip s) =
    toEncoding (map ActiveLamp $ Map.toList s)
