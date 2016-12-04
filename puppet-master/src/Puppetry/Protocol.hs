{-# LANGUAGE DeriveGeneric #-}
module Puppetry.Protocol
  ( Color(..)
  , ArraySelect(..)
  , Target(..)
  , PuppetCommand(..)
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           GHC.Generics    (Generic)
import           GHC.Word ()

data Color = Color
  { red   :: !Word8
  , green :: !Word8
  , blue  :: !Word8
  , fase  :: !Word8
  } deriving (Show, Read, Generic)

instance Binary Color

data ArraySelect = ArraySelect
  { backlight  :: !Bool
  , midlight   :: !Bool
  , frontlight :: !Bool
  , scenelight :: !Bool
  , sidelight  :: !Bool
  } deriving (Show, Read)

getArraySelect :: Bits a => a -> ArraySelect
getArraySelect w =
  ArraySelect
  { backlight = testBit w 0
  , midlight = testBit w 1
  , frontlight = testBit w 2
  , scenelight = testBit w 3
  , sidelight = testBit w 4
  }

putArraySelect :: Bits a => ArraySelect -> a -> a
putArraySelect t =
  putBitList $ [backlight, midlight, frontlight, scenelight, sidelight] <*> [t]

putBitList :: Bits a => [Bool] -> a -> a
putBitList ls = go ls 0
  where
    go (x:xs) i =
      setLast . go xs (i + 1)
      where
        setLast a | x = setBit a i
        setLast a = a
    go [] _ = id

getBitList :: Bits a => a -> Int -> [Bool]
getBitList a i = go 0
  where
    go j | j < i = testBit a j : go (j + 1)
    go _ = []

data Target = Target
  { arrays :: !ArraySelect
  , pixels :: ![Bool]
  } deriving (Show, Read)

instance Binary Target where
  get = do
    w <- getWord32be
    return $ Target
      { arrays = getArraySelect w
      , pixels = getBitList (shift w (-5)) 27
      }
  put (Target as px) = do
    putWord32be . putArraySelect as $ shift (putBitList px zeroBits) 5


data PuppetCommand
  = Set Target Color
  | Gradient Word32 Target Color
  deriving (Show, Read)

instance Binary PuppetCommand where
  get = do
    w <- getWord8
    case w of
      0x00 -> Set <$> get <*> get
      0x01 -> Gradient <$> get <*> get <*> get
      _ -> error "Undefined Command"
  put cmd =
    case cmd of
      Set t c -> do
        putWord8 0x00
        put t
        put c
      Gradient ms t c -> do
        putWord8 0x01
        put ms
        put t
        put c
