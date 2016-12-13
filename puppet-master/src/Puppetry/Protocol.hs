{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Puppetry.Protocol
  ( Color(..)
  , ArraySelect(..)
  , Target(..)
  , PuppetCommand(..)
  , PuppetResponse(..)
  , runPuppetry
  , defaultPuppetrySettings
  , printProgram

  , sendP
  , set
  , gradient
  , check

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
import           Control.Monad.Free

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
putBitList ls a' = go ls 0 a'
  where
    lenghtA = a'
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


data PuppetResponse
  = AllOk
  | Error String

instance Binary PuppetResponse where
  get = do
    w <- getWord8
    case w of
      0x00 ->
        return AllOk
      otherwise ->
        return . Error $ "Could not parse '" ++ show w ++ "'"
  put res =
    case res of
      AllOk ->
        putWord8 0x00

data PuppetCommand
  = Set Target Color
  | Gradient Word32 Target Color
  | Check
  deriving (Show, Read)

(&:) :: Binary b => Put -> b -> Put
p &: m = p >> put m

instance Binary PuppetCommand where
  get = do
    w <- getWord8
    case w of
      0x00 -> Set <$> get <*> get
      0x01 -> Gradient <$> get <*> get <*> get
      0x02 -> return Check
      _ -> error "Undefined Command"
  put cmd =
    case cmd of
      Set t c ->
        putWord8 0x00 &: t &: c
      Gradient ms t c ->
        putWord8 0x01 &: ms &: t &: c
      Check ->
        putWord8 0x02

{- for now we just use the default settings, which is
  * 9600 baud
  * 8 data bits
  * 1 stop bit
  * no parity
  * no flow control
  * 0.1 second receive timeout
-}
defaultPuppetrySettings :: SerialPortSettings
defaultPuppetrySettings =
  defaultSerialSettings

{- A PuppetProgram is sending a PuppetCommand, and depending on the
response will know what command to send next. -}
data PuppetProgram next
  = SendP PuppetCommand (PuppetResponse -> next)
  deriving (Functor)

{- Implements the puppet program using a free monad. A free monad is
a cool construct that separates the construction of the monad from
the interpretation.
-}
type PuppetM = Free PuppetProgram

{- The interpreter, sends all commands to  -}
runPuppetry :: FilePath -> SerialPortSettings -> PuppetM a -> IO a
runPuppetry fp sps m =
  withSerial fp sps (doSerial m)
  where
    doSerial (Free (SendP cmd next)) sp = do
       -- TODO: Not completly safe, might not send the entire string
       send sp . BL.toStrict $ encode cmd
       rpl <- recvResponse sp
       case rpl of
         Error str ->
           -- If the cmd is Check, don't fail.
           case cmd of
             Check ->
               doSerial (next rpl) sp
             otherwise ->
               error str
         otherwise -> doSerial (next rpl) sp
    doSerial (Pure a) sp =
      return a

    {- TODO: Unsafe IO, we always expect input. -}
    recvResponse :: SerialPort -> IO PuppetResponse
    recvResponse sp = do
      bs <- recv sp 1
      case decodeOrFail $ BL.fromStrict bs of
        Left (bs, bo, s) ->
          error "Something technical happend"
        Right (bs, bo, e) ->
          return e

printProgram :: PuppetM a -> IO a
printProgram (Pure a) = return a
printProgram (Free (SendP cmd next)) = do
  print cmd
  printProgram (next AllOk)

sendP :: PuppetCommand -> PuppetM ()
sendP = liftF . flip SendP (const ())

set :: Target -> Color -> PuppetM ()
set t c = sendP $ Set t c

gradient :: Word32 -> Target -> Color -> PuppetM ()
gradient ms t c = sendP $ Gradient ms t c

check :: PuppetM Bool
check = liftF $ SendP Check (\case AllOk -> True; otherwise -> False)
