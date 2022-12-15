{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)

-- import qualified Control.Concurrent as Concurrent

-- import qualified Control.Concurrent as Concurrent

-- import qualified Control.Concurrent as Concurrent

-- import qualified Control.Concurrent as Concurrent

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Puppetry.Color
import Puppetry.State
import Puppetry.Transfer (readToBang, stateToString, transferS)
import System.Hardware.Serialport (
  SerialPortSettings (..),
  defaultSerialSettings,
  withSerial,
 )

serialSettings :: SerialPortSettings
serialSettings = defaultSerialSettings{timeout = 10}

main :: IO ()
main = do
  getArgs >>= \case
    ["-"] -> do
      run (\s -> do putStrLn (stateToString s))
    [uport] -> do
      withSerial uport serialSettings $ \sp -> do
        run
          ( \s -> do
              transferS s sp
              readToBang sp
          )
    _ -> error "unexpected"

run :: (State -> IO ()) -> IO ()
run send = do
  send darkness
  wait
  sendOver 2.0 (introes 20 sunrise)
  wait
  send lights
  wait
  sendOver 2.0 (fadeout 20 sunset)
 where
  sendOver _ [] = error "not expected"
  sendOver (sec :: Float) (a : as) = do
    send a; sendDelayed delay as
   where
    ln = length as
    delay = round (1_000_000 * sec / fromIntegral ln)

  sendDelayed delay = \case
    (a : as) -> do
      threadDelay delay
      send a
      sendDelayed delay as
    [] -> return ()

fadeout :: Int -> State -> [State]
fadeout n s2 = reverse $ introes n s2

introes :: Int -> State -> [State]
introes n s2 =
  [ mapColorsState (dim (fromIntegral x / fromIntegral n)) s2
  | x <- [0 .. n - 1]
  ]
 where
  dim (f :: Float) c =
    c
      { red = round (fromIntegral (red c) * f)
      , blue = round (fromIntegral (blue c) * f)
      , green = round (fromIntegral (green c) * f)
      }

mapColorsState :: (Color -> Color) -> State -> State
mapColorsState fn s =
  State
    { back = mapColor fn (back s)
    , middle = mapColor fn (middle s)
    , front = mapColor fn (front s)
    , left = mapColor fn (left s)
    , right = mapColor fn (right s)
    , proscenium = mapColor fn (proscenium s)
    }

wait :: IO ()
wait = do
  putStrLn "Press Enter to Continue"
  void getLine
  putStrLn "Thanks!"

darkness :: State
darkness = emptyState

monochrome :: Color -> State
monochrome c =
  State
    { back = fromList [(0, c)]
    , middle = fromList [(0, c)]
    , front = fromList [(0, c)]
    , left = fromList [(0, c)]
    , right = fromList [(0, c)]
    , proscenium = fromList [(0, c)]
    }

lights :: State
lights = monochrome cWhite

sunrise :: State
sunrise =
  emptyState
    { back = fromList [(0, cBlack), (10, cBlack), (26, sunColor)]
    , right = fromList [(0, sunColor)]
    }
 where
  sunColor = cRed

sunset :: State
sunset =
  emptyState
    { back = fromList [(0, sunColor), (16, cBlack), (26, cBlack)]
    , left = fromList [(0, sunColor)]
    }
 where
  sunColor = cRed
