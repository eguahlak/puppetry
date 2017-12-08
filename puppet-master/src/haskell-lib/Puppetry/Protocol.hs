{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Puppetry.Protocol
  ( Color(..)
  , ArraySelect(..)
  , Target(..)
  , PuppetCommand(..)
  , PuppetResponse(..)
  , PuppetM (..)
  , runPuppetry
  , unsafeRunPuppetry
  , defaultPuppetrySettings
  , printProgram
  , printAll
  , execute

  , withSerial

  , sendP
  , sendCmd
  , nop
  , set
  , gradient
  , test

  , leftSide
  , rightSide

  , everything
  , nothing
  , noArrays
  , allArrays
  , black
  , cRed
  , cBlue
  , cGreen
  ) where

import           Debug.Trace
import           System.Posix.Unistd

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put  hiding (flush)
import           Data.Bits
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL

import           Data.Aeson                 (FromJSON (..), Object, Value (..),
                                             (.:), (.:?))

import           Data.String
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Numeric

import           GHC.Generics               (Generic)
import           GHC.Word                   ()

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Plus
import           Control.Monad.IO.Class
import           Control.Concurrent

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

instance FromJSON Color where
  parseJSON (String t) =
    case readHex (T.unpack t) of
      [(i, "")] ->
        return $ decode (runPut (putWord32be i))
      _ -> mzero
  parseJSON _ = mzero

data ArraySelect = ArraySelect
  { backlight  :: !Bool
  , midlight   :: !Bool
  , frontlight :: !Bool
  , scenelight :: !Bool
  , sidelight  :: !Bool
  } deriving (Show, Read, Generic)

instance Monoid ArraySelect where
  mempty = noArrays
  mappend m1 m2 =
    ArraySelect
     { backlight = backlight m1 || backlight m2
     , midlight = midlight m1 || midlight m2
     , frontlight = frontlight m1 || frontlight m2
     , scenelight = scenelight m1 || scenelight m2
     , sidelight = sidelight m1 || sidelight m2
     }

instance FromJSON ArraySelect where
  parseJSON (String "all") =
    return allArrays
  parseJSON (String "mid") =
    return $ noArrays { midlight = True }
  parseJSON (String "back") =
    return $ noArrays { backlight = True }
  parseJSON (String "front") =
    return $ noArrays { frontlight = True }
  parseJSON (String "side") =
    return $ noArrays { sidelight = True }
  parseJSON (String "scene") =
    return $ noArrays { scenelight = True }
  parseJSON (Array a) = do
    items <- sequence . map parseJSON $ V.toList a
    return . mconcat $ items
  parseJSON _ = mzero

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

leftSide = map (\x -> mod x 2 == 0) [0..26]

rightSide = map (\x -> mod x 2 == 1) [0..26]

data Target = Target
  { arrays :: !ArraySelect
  , pixels :: ![Bool]
  } deriving (Show, Read, Generic)

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

instance FromJSON Target where
  parseJSON (Object o) = do
    a <- o .: "arrays"
    msum
      [ do
        px :: T.Text <- o .: "pixels"
        case px of
          "left" -> return $ Target a leftSide
          "right" -> return $ Target a rightSide
          "all" -> return . Target a $ replicate 27 True
          othervise -> mzero
      , do
        px <- o .: "pixels"
        return $ Target a px
      ]

  parseJSON (String "all") =
    return $ everything

  parseJSON (String "left") =
    return $ everything { pixels = leftSide }

  parseJSON (String "right") =
    return $ everything { pixels = rightSide }

  parseJSON _ = mzero

data PuppetResponse
  = AllOk
  | Error String

instance Binary PuppetResponse where
  get = do
    w <- getWord8
    case w of
      0x21 ->
        return AllOk
      0x3F ->
        return . Error $ "Bad Command"
      otherwise ->
        return . Error $ "Could not parse '" ++ show w ++ "'"
  put res =
    case res of
      AllOk ->
        putWord8 0x21

data PuppetCommand
  = NOp
  | Set Target Color
  | Gradient Word32 Target Color
  | Test
  deriving (Show, Read)

instance FromJSON PuppetCommand where
  parseJSON (Object o) = do
    cmd <- o .: "cmd"
    case (cmd :: T.Text) of
      "nop"   -> return NOp
      "gradient" -> do
        time <- o .: "time"
        target <- o .: "target"
        color <- o .: "color"
        return $ Gradient time target color
      "set" -> do
        target <- o .: "target"
        color <- o .: "color"
        return $ Set target color
      "test" -> return Test
      _ -> mzero
  parseJSON _ = mzero

(&:) :: Binary b => Put -> b -> Put
p &: m = p >> put m

instance Binary PuppetCommand where
  get = do
    w <- getWord8
    case w of
      0x00 -> return NOp
      0x53 -> Set <$> get <*> get
      0x47 -> Gradient <$> get <*> get <*> get
      0x54 -> return Test
      _    -> error "Undefined Command"
  put cmd =
    case cmd of
      NOp ->
        putWord8 0x00
      Set t c ->
        putWord8 0x53 &: t &: c
      Gradient ms t c ->
        putWord8 0x47 &: ms &: t &: c
      Test ->
        putWord8 0x54

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
  defaultSerialSettings { timeout = 100 }

{- A PuppetProgram is sending a PuppetCommand, and depending on the
response will know what command to send next. -}
data PuppetProgram next
  = SendP PuppetCommand (PuppetResponse -> next)
  | SendCmd BS.ByteString next
  | DoIO (IO next)
  deriving (Functor)

{- Implements the puppet program using a free monad. A free monad is
a cool construct that separates the construction of the monad from
the interpretation.
-}
type PuppetM = Free PuppetProgram

instance MonadIO (Free PuppetProgram) where
  liftIO m = liftF (DoIO m)

data PuppetInstruction
  = PSend PuppetCommand
  | PWait Int
  deriving (Show, Read)

instance FromJSON PuppetInstruction where
  parseJSON (Object o) = do
    inst :: Maybe T.Text <- o .:? "inst"
    case inst of
      Just "wait" ->
        PWait <$> o .: "ms"
      otherwise ->
        PSend <$> parseJSON (Object o)

execute :: [PuppetInstruction] -> PuppetM ()
execute [] = return ()
execute (a:rest) = do
  case a of
    PSend p -> sendP p
    PWait ms ->
      liftIO $ do { putStrLn $ "Started Waiting"
                  ; threadDelay (ms * 1000)
                  ; putStrLn $ "Waiting " ++ show ms
                  }
  execute rest

unsafeRunPuppetry :: SerialPort -> PuppetM a -> IO a
unsafeRunPuppetry sp puppet =
  case puppet of
    Free (DoIO m) -> do
      next <- m
      unsafeRunPuppetry sp next

    Free (SendP cmd next) -> do
      send sp . traceShowId . BL.toStrict $ encode cmd
      rpl <- recvResponse sp
      case rpl of
        Error str ->
          -- If the cmd is C
          case cmd of
            Test ->
              unsafeRunPuppetry sp $ next rpl
            otherwise ->
              error str
        otherwise -> unsafeRunPuppetry sp $ next rpl

    Pure a ->
      return a

  where
  recvResponse :: SerialPort -> IO PuppetResponse
  recvResponse sp = do
    bs <- recv sp 1
    print bs
    if 0 == BS.length bs
      then recvResponse sp
      else
        case decodeOrFail $ BL.fromStrict bs of
          Left a ->
            error $ "Something technical happend: " ++ show a
          Right (bs, bo, e) ->
            return e

printAll sp = do
  bs <- recv sp 1
  if BS.length bs > 0 && bs /= "!" then do
    BS.putStr bs
    printAll sp
  else
    BS.putStr bs

{- The interpreter, sends all commands to  -}
runPuppetry :: FilePath -> SerialPortSettings -> PuppetM a -> IO a
runPuppetry fp sps m =
  withSerial fp sps $ \sp -> do
    printAll sp -- print everything left in the thing.
    unsafeRunPuppetry sp m

printProgram :: PuppetM a -> IO a
printProgram (Pure a) = return a
printProgram (Free (SendP cmd next)) = do
  print cmd
  printProgram (next AllOk)
printProgram (Free (SendCmd cmd next)) = do
  print cmd
  printProgram next
printProgram (Free (DoIO m)) = do
  next <- m
  printProgram next

sendP :: PuppetCommand -> PuppetM ()
sendP = liftF . flip SendP (const ())

sendCmd :: String -> PuppetM ()
sendCmd = liftF . flip SendCmd () . fromString

nop :: Target -> Color -> PuppetM ()
nop t c = sendP $ NOp

set :: Target -> Color -> PuppetM ()
set t c = sendP $ Set t c

gradient :: Word32 -> Target -> Color -> PuppetM ()
gradient ms t c = sendP $ Gradient ms t c

test :: PuppetM Bool
test = liftF $ SendP Test (\case AllOk -> True; otherwise -> False)
