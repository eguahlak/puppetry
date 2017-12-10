{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import           Control.Lens
import qualified Control.Monad                  as Monad
import           Control.Monad.Reader
import           Control.Monad.State            (StateT, get, modify, put,
                                                 runStateT)
import qualified Data.List                      as List
import           Data.Monoid
import           Data.Semigroup                 hiding ((<>))

import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as WSS
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

import           Data.Aeson                     (FromJSON, ToJSON, decode,
                                                 encode)

import           Puppetry.State

type ClientId = Int
type Client   = (ClientId, WS.Connection)

type StateRef = Concurrent.MVar ServerState
data ServerState = ServerState
  { _clientList :: ![Client]
  , _lights     :: !State
  }

makeLenses 'ServerState

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
    (s, a) <- runStateT m s
    return (a, s)


main :: IO ()
main = do
  stateRef <- Concurrent.newMVar (ServerState [] exampleState)
  let port = 3000
  putStrLn $ "Starting puppet-master at " ++ show port
  Warp.run port $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp stateRef)
    httpApp

httpApp :: Wai.Application
httpApp =
  WSS.staticApp (WSS.defaultFileServerSettings "public")

wsApp :: StateRef -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  client <- runReaderT (connectClient conn) stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (runReaderT (listen client) stateRef)
    (runReaderT (disconnectClient client) stateRef)

-- | Listen on a client, if we receive a state from the client
-- we update and broadcast.
listen :: Client -> PuppetServer ()
listen client =
  Monad.forever $ do
    msg <- liftIO $ recv client
    case msg of
      Just state -> do
        -- Lock state until broadcast is completed
        atomic $ do
          lights .= state
          clients <- use clientList
          liftIO $ multisend state clients
      Nothing ->
        return ()

-- | Receive data from the client
recv :: FromJSON a => Client -> IO (Maybe a)
recv (_, conn) =
  decode <$> liftIO (WS.receiveData conn)

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
    st <- use lights
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
