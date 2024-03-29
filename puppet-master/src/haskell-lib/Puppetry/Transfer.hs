{-# LANGUAGE BlockArguments #-}

module Puppetry.Transfer where

import Puppetry.Color
import Puppetry.State

import GHC.IO.Handle
import System.Hardware.Serialport

import qualified Data.ByteString.Char8 as B

import Data.List

import Control.Monad (unless)
import Numeric

data StripName
  = StripBack
  | StripMiddle
  | StripFront
  | StripLeft
  | StripRight
  | StripProscenium
  deriving (Show, Eq, Ord)

stripAsChar :: StripName -> Char
stripAsChar s =
  case s of
    StripBack -> 'B'
    StripMiddle -> 'M'
    StripFront -> 'F'
    StripLeft -> 'L'
    StripRight -> 'R'
    StripProscenium -> 'P'

data Lamp = Lamp
  { strip :: StripName
  , index :: Int
  , color :: Color
  }
  deriving (Show, Eq)

stateToString :: State -> String
stateToString s =
  let str = map ((++ "\n") . lampToString) (toLampList s) ++ ["!"]
   in intercalate "" str

transferS :: State -> SerialPort -> IO ()
transferS state sp = do
  _ <- send sp (B.pack $ stateToString state)
  flush sp

transfer :: Handle -> State -> IO ()
transfer h sts = do
  hPutStr h $ stateToString sts
  hFlush h

readToBang :: SerialPort -> IO ()
readToBang sp = do
  c <- B.unpack <$> recv sp 1
  putStr c
  unless (c == "") do
    readToBang sp

toLampList :: State -> [Lamp]
toLampList s =
  withName StripBack back
    ++ withName StripMiddle middle
    ++ withName StripFront front
    ++ withName StripLeft left
    ++ withName StripRight right
    ++ withName StripProscenium proscenium
 where
  withName n f =
    map (uncurry (Lamp n)) (toList $ f s)

lampToString :: Lamp -> String
lampToString l =
  intercalate
    ""
    [ [stripAsChar (strip l)]
    , i2hx index
    , i2hx (red . color)
    , i2hx (blue . color)
    , i2hx (green . color)
    ]
 where
  i2hx f =
    exact 2 '0' $ reverse $ showHex (f l) ""

exact :: Int -> a -> [a] -> [a]
exact i a as =
  fill $ take i (reverse as)
 where
  fill l
    | length l < i = fill (a : l)
    | otherwise = l
