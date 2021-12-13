{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE LambdaCase  #-}

module Main where

import Paths_puppet_master as Paths

-- directory
import           System.Directory

-- filepath
import           System.FilePath

-- base
import           GHC.Generics                   (Generic)

-- containers
import qualified Data.Map                       as Map

import qualified Data.ByteString.Lazy           as BL

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import           Control.Lens                   hiding ((<.>))
import qualified Control.Monad                  as Monad
import           Control.Monad.Reader
import           Control.Monad.State            (StateT, runStateT)
import           Data.Aeson hiding ((.=))
import qualified Data.List                      as List
import           Data.Semigroup                 hiding ((<>))
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as WSS
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import           System.Environment
import           System.Hardware.Serialport     (SerialPort,
                                                 SerialPortSettings (..),
                                                 defaultSerialSettings,
                                                 withSerial)
import           System.IO

import           Puppetry.State
import           Puppetry.Transfer
import           Puppetry.Color

type ClientId = Int
type Client   = (ClientId, WS.Connection)

data ClientState = ClientState
  { _lights      :: !State
  , _savedStates :: !(Map.Map Int Color)
  } deriving (Generic)

dropOne :: Options
dropOne = defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ClientState where
  parseJSON = genericParseJSON dropOne

instance ToJSON ClientState where
  toJSON = genericToJSON dropOne
  toEncoding = genericToEncoding dropOne

type StateRef = Concurrent.MVar ServerState
data ServerState = ServerState
  { _clientList  :: ![Client]
  , _saveFolder  :: !FilePath
  , _serialport  :: Maybe (FilePath, SerialPort)
  , _clientState :: !ClientState
  }

makeLenses 'ServerState
makeLenses 'ClientState

type PuppetServer = ReaderT StateRef IO
type Puppetry = StateT ServerState IO

-- | Execute an atomic access to the ServerState. Be aware, this is a
-- locking action.
atomic
  :: Puppetry a
  -> PuppetServer a
atomic m = do
  ref <- ask
  liftIO . Concurrent.modifyMVar ref $ \ s -> do
    (s', a) <- runStateT m s
    return (a, s')

initializeSavedStates :: FilePath -> IO (Map.Map Int Color)
initializeSavedStates folder = do
  createDirectoryIfMissing True folder
  files <- listDirectory folder
  states <- forM files $ \file -> do
    let i = read . takeBaseName $ file
    state <- eitherDecode' <$> BL.readFile (folder </> file)
    return (i, either (const cBlack) average $ state)
  return $ Map.fromList states


sInit :: FilePath -> Maybe (FilePath, SerialPort) -> ClientState -> ServerState
sInit fp sp cst =
  ServerState
    []
    fp
    sp
    cst

serialSettings :: SerialPortSettings
serialSettings =
  defaultSerialSettings { timeout = 10 };

-- | main is run with
-- puppet-master <port> <usbport> <save-states-folder>
main :: IO ()
main = do
  getArgs >>= \case
    [strPort, uport, folder] -> do
      savedStates' <- initializeSavedStates folder
      let state = ClientState exampleState savedStates'

      let port = read strPort
      putStrLn $ "Starting puppet-master at " ++ show port
      if uport /= "-"
        then
          withSerial uport serialSettings $ \sp -> do
            stateRef <- Concurrent.newMVar
              (sInit folder (Just (uport, sp)) state)
            run port stateRef
        else do
          stateRef <- Concurrent.newMVar (sInit folder Nothing state)
          run port stateRef
    _ -> putStrLn "puppet-master <port> <usbport> <saved-states-folder>"

  where
    run port stateRef = do
      -- fp <- Paths.getDataDir
      fp <- canonicalizePath "public"
      putStrLn $ "Serving: " <> fp
      Warp.run port $ WS.websocketsOr
        WS.defaultConnectionOptions
        (wsApp stateRef)
        (httpApp fp)


httpApp :: FilePath -> Wai.Application
httpApp fp =
  WSS.staticApp (WSS.defaultFileServerSettings fp)

wsApp :: StateRef -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  client <- runReaderT (connectClient conn) stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (runReaderT (listen client) stateRef)
    (runReaderT (disconnectClient client) stateRef)


data Protocol
  = Save !Int
  | Load !Int
  | UpdateState !State
  deriving (Show, Read, Generic)

instance FromJSON Protocol

-- | Listen on a client, if we receive a state from the client
-- we update and broadcast.
listen :: Client -> PuppetServer ()
listen client = do
  Monad.forever $ do
    msg <- liftIO $ recv client
    liftIO $ putStrLn $ "Received message from: " ++ show (client ^. _1)
    case msg of
      Right msg' ->
        atomic $
          case msg' of
            UpdateState state ->
              updateState state
            Load no -> do
              state' <- loadState no
              case state' of
                Left err -> do
                  liftIO . putStrLn $
                    "Could not load " ++ show no ++ ": " ++ err
                  clientState.savedStates.at no .= Just (average emptyState)
                  updateState emptyState
                Right state -> do
                  clientState.savedStates.at no .= Just (average state)
                  updateState state
            Save no ->
              saveState no =<< use (clientState.lights)
      Left err -> do
        liftIO $ putStrLn ("Error in conversion: " ++ err)

  where
    updateState lights' = do
      clientState.lights .= lights'
      printState
      state <- use clientState
      clients <- use clientList
      liftIO $ multisend state clients

loadState :: Int -> Puppetry (Either String State)
loadState i = do
  file <- getSaveFile i
  liftIO
    . Exception.handle (\(_ :: Exception.IOException) -> return $ Left $ "No such file: " ++ file)
    $ eitherDecode' <$> BL.readFile file

saveState :: Int -> State -> Puppetry ()
saveState i state = do
  file <- getSaveFile i
  clientState.savedStates.at i .= Just (average state)
  liftIO $ BL.writeFile file (encode state)

getSaveFile :: Int -> Puppetry FilePath
getSaveFile i = do
  f <- use saveFolder
  return (f </> show i <.> "json")

printState :: Puppetry ()
printState = do
    s <- use (clientState.lights)
    p <- use serialport
    liftIO $ do
      transfer stdout s
    case p of
      Nothing -> return ()
      Just (_, sp) ->
        liftIO $ do
            transferS s sp
            readToBang sp
          -- h <- hOpenSerial p'
          -- transfer h s
          -- str <- readToBang h
          -- putStrLn $ "Response: '" ++ str ++ "'"
          -- hClose h

-- | Receive data from the client
recv :: FromJSON a => Client -> IO (Either String a)
recv (_, conn) =
  eitherDecode' <$> liftIO (WS.receiveData conn)

-- | Broadcast the state to list clients.
multisend :: ToJSON a => a -> [Client] -> IO ()
multisend a clients = do
  let txt = encode a
  Monad.forM_ clients $ \(_, conn) ->
    WS.sendTextData conn txt

-- | Send an message to a single client
send :: ToJSON a => a -> Client -> IO ()
send a (_, conn) = do
  let txt = encode a
  WS.sendTextData conn txt

-- | Connect a client, atomically add the client to the list
-- and send out the current state.
connectClient :: WS.Connection -> PuppetServer Client
connectClient conn =
  atomic $ do
    client <- addClient conn
    st <- use clientState
    liftIO $ putStrLn $ "Received new client: " ++ show (client ^. _1)
    liftIO $ send st client
    clients <- use clientList
    liftIO $ putStrLn $ "Client list is: " ++ show (map (^. _1) clients)
    return client

addClient :: WS.Connection -> Puppetry Client
addClient conn = do
  Max clientId <- (Max 0 <>) <$> (use $ clientList . traverse . _1 . to Max)
  let client = (clientId + 1, conn)
  clientList %= (client:)
  return client

-- | Disconnect the client
disconnectClient :: Client -> PuppetServer ()
disconnectClient client =
  atomic $ clientList %= withoutClient client

-- | Returns a list of clients without the client
withoutClient :: Client -> [Client] -> [Client]
withoutClient (clientId, _) = List.filter ((/=) clientId . fst)
