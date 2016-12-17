{-# LANGUAGE OverloadedStrings #-}
module Puppetry.Server (startPuppetServer) where

import           Data.Aeson                    (Value (..), object, (.=))
import           Network.Wai                   (Application)
import           Network.Wai.Middleware.Static
import qualified Web.Scotty.Trans              as S


import qualified Data.List as L

import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Lazy
import           System.Posix

import qualified Control.Concurrent.Lock as Lock

import           Puppetry.Protocol

startPuppetServer :: FilePath -> Int -> IO ()
startPuppetServer fp port = do
  putStrLn "Starting Puppet Server"
  unsafe
  where
    unsafe :: IO ()
    unsafe = do
      lock <- Lock.new
      withSerial fp defaultPuppetrySettings $ \sp -> do
        printAll sp
        S.scottyT port (Lock.with lock . unsafeRunPuppetry sp) app
    safe :: IO ()
    safe =
      S.scottyT port (runPuppetry fp defaultPuppetrySettings) app

app :: S.ScottyT Text PuppetM ()
app = do
  S.middleware $ staticPolicy (noDots >-> addBase "../puppet-client")
  S.get "/" $ do
    S.file "../puppet-client/index.html"
    S.setHeader "Content-Type" "text/html"

  S.get "/jquey.js" $ do
    S.file "../puppet-client/jquery.js"
    S.setHeader "Content-Type" "application/javascript"

  S.post "/api" $ do
    cmds <- S.jsonData
    lift $ execute cmds

  S.put "/api/send" $ do
    cmd <- S.jsonData
    lift $ sendP cmd

