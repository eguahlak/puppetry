{-# LANGUAGE OverloadedStrings #-}
module Puppetry.Server (startPuppetServer) where

import           Data.Aeson                    (Value (..), object, (.=))
import           Network.Wai                   (Application)
import           Network.Wai.Middleware.Static
import qualified Web.Scotty.Trans              as S

import           Control.Monad.Trans (lift)
import           Data.Text.Lazy

import           Puppetry.Protocol

startPuppetServer :: FilePath -> Int -> IO ()
startPuppetServer fp port = do
  runPuppetry fp defaultPuppetrySettings $ do
    test
    set everything cRed

  -- putStrLn "Starting Puppet Server"
  -- S.scottyT port (runPuppetry fp defaultPuppetrySettings) app

app :: S.ScottyT Text PuppetM ()
app = do
  S.middleware $ staticPolicy (noDots >-> addBase "../puppet-client")
  S.get "/" $ do
    S.file "../puppet-client/index.html"
    S.setHeader "Content-Type" "html"

  S.put "/api/send" $ do
    cmd <- S.jsonData
    lift $ sendP cmd
