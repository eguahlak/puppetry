{-# LANGUAGE LambdaCase #-}

import qualified Control.Concurrent as Concurrent
import System.Hardware.Serialport (
  SerialPort,
  SerialPortSettings (..),
  defaultSerialSettings,
  withSerial,
 )

main :: IO ()
main = do
  getArgs >>= \case
    [uport] -> do
      withSerial uport serialSettings $ \sp -> do
        if uport /= "-"
          then withSerial uport serialSettings $ \sp -> do
            stateRef <-
              Concurrent.newMVar
                (sInit folder (Just (uport, sp)) state)
            run port stateRef
          else do
            stateRef <- Concurrent.newMVar (sInit folder Nothing state)
            run port stateRef

run stateRef = do
