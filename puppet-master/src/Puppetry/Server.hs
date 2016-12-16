{-# LANGUAGE OverloadedStrings #-}
module Puppetry.Server (startPuppetServer) where

import           Data.Aeson                    (Value (..), object, (.=))
import           Network.Wai                   (Application)
import           Network.Wai.Middleware.Static
import qualified Web.Scotty.Trans              as S

import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Lazy
import     System.Posix

import           Puppetry.Protocol

startPuppetServer :: FilePath -> Int -> IO ()
startPuppetServer fp port = do
  runPuppetry fp defaultPuppetrySettings $ do
    test
    set (everything { arrays = noArrays { midlight = True }, pixels = leftSide })
         cGreen
    set (everything { arrays = noArrays { midlight = True }, pixels = rightSide })
         cRed
    -- set (everything { arrays = noArrays { scenelight = True }}) cRed
    -- set (everything { arrays = noArrays { backlight = True }}) cBlue
    -- set (everything { arrays = noArrays { midlight = True }}) cGreen
    -- set (everything { arrays = noArrays { frontlight = True }}) cRed
    -- set (everything { arrays = noArrays { sidelight = True }}) cGreen
    -- set nothing { arrays = noArrays { scenelight = True}} cRed

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
