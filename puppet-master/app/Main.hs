module Main where

import  Puppetry.Protocol
import  Puppetry.Server
import  System.Environment

main :: IO ()
main = do
  file:[] <- getArgs
  startPuppetServer file 8080
