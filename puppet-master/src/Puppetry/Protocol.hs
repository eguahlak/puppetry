{-# LANGUAGE DeriveGeneric #-}
module Puppetry.Protocol
  ( Color(..)
  , ArraySelect(..)
  , Target(..)
  , PuppetCommand(..)
  , PuppetResponds(..)
  , runPuppetry
  , defaultPuppetrySettings

  , sendCmd
  , set
  , gradient

  , everything
  , nothing
  , noArrays
  , allArrays
  , black
  , cRed
  , cBlue
  , cGreen
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString.Lazy as BL

import           GHC.Generics    (Generic)
import           GHC.Word ()

import           Control.Monad.IO.Class

import           System.Hardware.Serialport

data Color = Color
  { fase  :: !Word8
  , red   :: !Word8
  , green :: !Word8
  , blue  :: !Word8
  } deriving (Show, Read, Generic)

black = Color { red = 0, green = 0, blue = 0, fase = 0 }
cRed = black { red = 255 }
cGreen = black { green = 255 }
cBlue = black { blue = 255 }

instance Binary Color

data ArraySelect = ArraySelect
  { backlight  :: !Bool
  , midlight   :: !Bool
  , frontlight :: !Bool
  , scenelight :: !Bool
  , sidelight  :: !Bool
  } deriving (Show, Read)

noArrays = ArraySelect
  { backlight = False
  , midlight = False
  , frontlight = False
  , scenelight = False
  , sidelight = False
  }

allArrays = ArraySelect
  { backlight = True
  , midlight = True
  , frontlight = True
  , scenelight = True
  , sidelight = True
  }

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

everything = Target
  { arrays = allArrays
  , pixels = replicate 27 True
  }

nothing = Target
  { arrays = noArrays
  , pixels = replicate 27 False
  }

instance Binary Target where
  get = do
    w <- getWord32be
    return $ Target
      { arrays = getArraySelect w
      , pixels = getBitList (shift w (-5)) 27
      }
  put (Target as px) = do
    putWord32be . putArraySelect as $ shift (putBitList px zeroBits) 5


data PuppetResponds
  = AllOk

instance Binary PuppetResponds where
  get = do
    w <- getWord8
    case w of
      0x00 -> return AllOk
      otherwise ->
        error $ "Could not parse '" ++ show w ++ "'"
  put res =
    case res of
      AllOk ->
        putWord8 0x00

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


newtype PuppetM a = PuppetM { doSerial :: SerialPort -> IO a }

runPuppetry :: FilePath -> SerialPortSettings -> PuppetM a -> IO a
runPuppetry fp sps m =
  withSerial fp sps (doSerial m)

instance Functor PuppetM where
  fmap f x = PuppetM $ \sp ->
    f <$> doSerial x sp

instance Applicative PuppetM where
  pure = PuppetM . const . return
  f <*> a = PuppetM $ \sp ->
    doSerial f sp <*> doSerial a sp

instance MonadIO PuppetM where
  liftIO m = PuppetM (const m)

instance Monad PuppetM where
  a >>= b = PuppetM $ \sp -> do
    x <- doSerial a sp
    doSerial (b x) sp
  return = pure

set :: Target -> Color -> PuppetM PuppetResponds
set t c = sendCmd $ Set t c

gradient :: Word32 -> Target -> Color -> PuppetM PuppetResponds
gradient ms t c = sendCmd $ Gradient ms t c

{- TODO: Unsafe IO, we always expect input. -}
recvResponse :: SerialPort -> IO PuppetResponds
recvResponse sp = do
  bs <- recv sp 1
  case decodeOrFail $ BL.fromStrict bs of
    Left (bs, bo, s) ->
      error "Something technical happend"
    Right (bs, bo, e) ->
      return e

{- TODO: Unsafe IO, we always expect that we send the entire message. -}
sendCmd :: PuppetCommand -> PuppetM PuppetResponds
sendCmd c = PuppetM $ \sp -> do
  send sp . BL.toStrict $ encode c
  recvResponse sp

defaultPuppetrySettings :: SerialPortSettings
defaultPuppetrySettings =
  defaultSerialSettings
